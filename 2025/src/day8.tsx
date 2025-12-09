import example from "./examples/8.txt?raw";
import { useContext, useRef, useEffect } from "react";
import { InputContext } from "./contexts";
import { renderApp } from "./App";
import { useQuery } from "@tanstack/react-query";
import * as THREE from "three";
import { Solutions } from "./Solutions";
import { parseLines } from "./parse";
import { Point3 } from "./point3";
import { SMap } from "./smap";
import { product } from "./lib";

async function solve(input: string) {
  console.log("Solving problem");
  console.log("Parse", parseInput(input));
  const points = parseInput(input);

  const part1Length = points.length > 50 ? 1000 : 10;

  const shortest: [[Point3, Point3], number][] = [];

  for (let i = 0; i < points.length; i++) {
    for (let j = 0; j < i; j++) {
      const p1 = points[i];
      const p2 = points[j];
      shortest.push([[p1, p2], p1.sqDist(p2)]);
    }
  }
  shortest.sort((a, b) => a[1] - b[1]);

  console.log("Short: ", shortest);

  const groups_part1 = new SMap<Point3, number>(Point3);
  let group_count = 0;
  for (const p of points) {
    group_count++;
    groups_part1.set(p, group_count);
  }

  console.log("groups: ", group_count);
  for (const [[p1, p2], _] of shortest.slice(0, part1Length)) {
    const p1_id = groups_part1.get(p1)!;
    const p2_id = groups_part1.get(p2);
    if (p1_id == p2_id) {
      continue;
    }

    for (const [p, group_id] of groups_part1) {
      if (group_id == p2_id) {
        groups_part1.set(p, p1_id);
      }
    }
  }

  const counts = new Map();
  for (const [_, group_id] of groups_part1) {
    counts.set(group_id, (counts.get(group_id) || 0) + 1);
  }

  console.log("Counts: ", counts);

  let connected = [...counts.values()];
  connected.sort((a, b) => a - b);
  connected = connected.slice(connected.length - 3);

  let part2 = 0;

  const groups_part2 = new SMap(Point3);
  group_count = 0;
  for (const p of points) {
    group_count++;
    groups_part2.set(p, group_count);
  }

  for (const [[p1, p2], _] of shortest) {
    const p1_id = groups_part2.get(p1);
    const p2_id = groups_part2.get(p2);
    if (p1_id == p2_id) {
      continue;
    }
    group_count--;
    for (const [p, group_id] of groups_part2) {
      if (group_id == p2_id) {
        groups_part2.set(p, p1_id);
      }
    }

    if (group_count == 1) {
      console.log("Break!");
      part2 = p1.x * p2.x;
      break;
    }
  }

  return {
    part1: product(connected),
    part2: part2,
    graph: {
      points,
      edges: shortest.slice(0, part1Length).map((item) => item[0]),
      groups: groups_part1,
    },
    output: connected,
  };
}

function parseInput(input: string): Point3[] {
  const lines = parseLines(input);
  return lines.map((x) => Point3.fromString(x));
}

function Solution() {
  const input = useContext(InputContext);
  const { data, isLoading } = useQuery({
    queryKey: ["day8", input],
    queryFn: () => solve(input),
  });

  const containerRef = useRef(null);

  useEffect(() => {
    if (!containerRef.current || !data) {
      return;
    }
    return renderScene(
      containerRef.current,
      { width: 500, height: 500 },
      data.graph,
    );
  }, [data?.graph]);

  if (isLoading || !data) {
    return <div>Loading</div>;
  }

  const { part1, part2, output } = data;

  return (
    <>
      <Solutions part1={part1} part2={part2} />
      <div>{JSON.stringify(output)}</div>
      <div ref={containerRef} style={{ width: "500px", height: "500px" }}></div>
    </>
  );
}

type Graph = {
  points: Point3[];
  edges: [Point3, Point3][];
  groups: SMap<Point3, number>;
};

function renderScene(
  container: HTMLDivElement,
  size: { width: number; height: number },
  { points, edges, groups }: Graph,
) {
  console.log("Rendering scene!");
  const scene = new THREE.Scene();
  scene.background = new THREE.Color(0x0a0a0a);

  const camera = new THREE.PerspectiveCamera(
    75,
    size.width / size.height,
    0.1,
    200000,
  );

  const center = new THREE.Vector3();
  points.forEach((point) => {
    center.x += point.x;
    center.y += point.y;
    center.z += point.z;
  });
  center.divideScalar(points.length);

  const nodeRadius = center.x / 100;

  console.log("center", center);
  console.log("Node radius: ", nodeRadius);

  // Renderer setup
  const renderer = new THREE.WebGLRenderer({ antialias: true });
  renderer.setSize(size.width, size.height);
  container.appendChild(renderer.domElement);

  const nodes: THREE.Mesh[] = [];

  for (let i = 0; i < points.length; i++) {
    const point = points[i];
    const group = groups.get(point) || 0;

    const geometry = new THREE.SphereGeometry(nodeRadius, 16, 16);
    const material = new THREE.MeshPhongMaterial({
      color: new THREE.Color(
        `hsl(${(group / points.length) * 360}, 100%, 60%)`,
      ),
      emissive: new THREE.Color(
        `hsl(${(group / points.length) * 360}, 70%, 30%)`,
      ),
      shininess: 100,
    });
    const sphere = new THREE.Mesh(geometry, material);

    sphere.position.set(point.x, point.y, point.z);

    scene.add(sphere);
    nodes.push(sphere);
  }

  for (let i = 0; i < edges.length; i++) {
    const [p1, p2] = edges[i];
    const group = groups.get(p1) || 0;
    const geometry = new THREE.BufferGeometry().setFromPoints([
      new THREE.Vector3(p1.x, p1.y, p1.z),
      new THREE.Vector3(p2.x, p2.y, p2.z),
    ]);
    const material = new THREE.LineBasicMaterial({
      color: new THREE.Color(
        `hsl(${(group / points.length) * 360}, 100%, 60%)`,
      ),
    });
    const line = new THREE.Line(geometry, material);
    scene.add(line);
  }

  // Lighting
  const ambientLight = new THREE.AmbientLight(0xffffff, 0.4);
  scene.add(ambientLight);

  const pointLight = new THREE.PointLight(0xffffff, 1, 100);
  pointLight.position.set(10, 10, 10);
  scene.add(pointLight);

  let animationId: number;
  const animate = () => {
    animationId = requestAnimationFrame(animate);

    const time = Date.now() * 0.0002;
    const radius = center.x * 2.4;
    camera.position.x = center.x + Math.cos(time) * radius;
    camera.position.y = center.y;
    camera.position.z = center.z + Math.sin(time) * radius;

    camera.lookAt(center);

    renderer.render(scene, camera);
  };

  animate();

  renderer.render(scene, camera);

  return () => {
    if (renderer.domElement) {
      container.removeChild(renderer.domElement);
    }
    cancelAnimationFrame(animationId);
  };
}

renderApp(8, example, <Solution />);
