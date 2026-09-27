import type { Metadata } from "next";
import { headers } from "next/headers";
import { NblApp } from "./NblApp";

export async function generateMetadata(): Promise<Metadata> {
  const requestHeaders = await headers();
  const host = requestHeaders.get("x-forwarded-host") ?? requestHeaders.get("host") ?? "localhost:3000";
  const protocol = requestHeaders.get("x-forwarded-proto") ?? (host.startsWith("localhost") ? "http" : "https");
  const image = `${protocol}://${host}/og.png`;
  return {
    title: "NBL Analytics",
    description: "Player and team performance analysis for the National Basketball League.",
    openGraph: { title: "NBL Analytics", description: "Player and team performance analysis for the National Basketball League.", images: [{ url: image, width: 1733, height: 907, alt: "NBL Analytics — player and team performance" }] },
    twitter: { card: "summary_large_image", title: "NBL Analytics", description: "Player and team performance analysis for the National Basketball League.", images: [image] },
  };
}

export default function Home() {
  return <NblApp />;
}
