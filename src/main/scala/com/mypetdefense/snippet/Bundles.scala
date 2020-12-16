package com.mypetdefense.snippet

import java.io.InputStreamReader

import net.liftweb.common._
import net.liftweb.http.LiftRules._
import net.liftweb.http._
import net.liftweb.util.Helpers._
import net.liftweb.util._

import scala.xml._

case class BundleInfo(
    name: String,
    checksumInFilename: Boolean,
    parts: List[String],
    version: String
)
object Bundles {
  val assetDomain = "txtback.co/mypetdefense"

  private def readResource(resourcePath: String): Box[String] = {
    for {
      url    <- LiftRules.getResource(resourcePath)
      reader <- tryo(new InputStreamReader(url.openStream, "UTF-8"))
    } yield {
      readWholeThing(reader)
    }
  }
  private def bundleVersionsFrom(data: String): (Boolean, Map[String, String]) = {
    var checksumInFilename = false
    val filesAndVersions =
      Map[String, String]() ++ data.split("\n").flatMap { line =>
        line.split("=") match {
          case Array("checksum-in-filename", value) =>
            checksumInFilename = value == "true"

            Nil
          case Array(bundle, version) => List((bundle, version))
          case _                      => List[(String, String)]()
        }
      }

    (checksumInFilename, filesAndVersions)
  }
  private def bundlesFrom(data: String): Map[String, List[String]] = {
    Map[String, List[String]]() ++ data.split("\n\n").flatMap { bundle =>
      bundle.split("\n").toList match {
        case bundle :: rest =>
          List((bundle, rest))
        case Nil =>
          List[(String, List[String])]()
      }
    }
  }
  private def bundlesForType(bundleType: String) = {
    Map[String, BundleInfo]() ++
      (for {
        bundleData <- readResource("/bundles/" + bundleType + ".bundle").toList
        bundleVersionData              = readResource("/bundles/" + bundleType + "-bundle-versions") openOr ""
        (checksumInFilename, versions) = bundleVersionsFrom(bundleVersionData)
        (bundle, parts) <- bundlesFrom(bundleData)
        version = versions.getOrElse(bundle, nextNum.toString)
      } yield {
        (bundle, BundleInfo(bundle, checksumInFilename, parts, version))
      })
  }

  private val scriptBundles = bundlesForType("javascript")
  private val styleBundles  = bundlesForType("stylesheet")

  def snippetHandlers: SnippetPF = {
    case List("script-bundle") => scriptBundle _
    case List("style-bundle")  => styleBundle _
  }

  /**
    * Looks up the passed bundle name, finds it in the list, sets up
    * the URI for it based on the given urlBase and file extension, and
    * passes the resulting URI to the tag generator for the bundle.
    *
    * If lookup for the bundle fails, returns a NodeSeq.Empty.
    */
  private def bundleTagFor(
      bundleList: Map[String, BundleInfo],
      urlBase: String,
      extension: String,
      tagGenerator: (String) => NodeSeq
  ) = {
    {
      for {
        name   <- S.attr("name")
        bundle <- bundleList.get(name)
      } yield {
        val bundleFilename =
          if (bundle.checksumInFilename)
            bundle.name + "-" + bundle.version + "." + extension
          else
            bundle.name + "." + extension + "?" + bundle.version

        tagGenerator(urlBase + "/" + bundleFilename)
      }
    } openOr {
      NodeSeq.Empty
    }
  }

  private def expandedTagsFor(
      bundleList: Map[String, BundleInfo],
      urlBase: String,
      tagGenerator: (String) => NodeSeq
  ) = {
    def filesForBundle(bundle: BundleInfo): List[String] = {
      bundle.parts.flatMap { fileOrBundle => expandedFilesFor(fileOrBundle) }
    }
    def expandedFilesFor(fileOrBundle: String): List[String] = {
      if (fileOrBundle.contains("."))
        fileOrBundle :: Nil
      else
        bundleList.get(fileOrBundle).toList.flatMap(filesForBundle(_))
    }

    for {
      name   <- S.attr("name").toList
      bundle <- bundleList.get(name).toList
      file   <- filesForBundle(bundle)
    } yield {
      tagGenerator(LiftRules.attachResourceId(urlBase + "/" + file))
    }
  }

  private def scriptBundle(ns: NodeSeq): NodeSeq = {
    def scriptTag(uri: String) = <script type="text/javascript" src={uri}></script>

    Props.mode match {
      case Props.RunModes.Development =>
        expandedTagsFor(scriptBundles, "/static/js", scriptTag _).flatten
      case _ =>
        bundleTagFor(scriptBundles, "/static/javascripts", "js", scriptTag _)
    }
  }

  private def styleBundle(ns: NodeSeq): NodeSeq = {
    def styleTag(uri: String) = <link type="text/css" rel="stylesheet" href={uri} />

    Props.mode match {
      case Props.RunModes.Development =>
        expandedTagsFor(styleBundles, "/static/css", styleTag _).flatten
      case _ =>
        bundleTagFor(styleBundles, "/static/stylesheets", "css", styleTag _)
    }
  }
}
