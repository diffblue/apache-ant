package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.resources.BZip2Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class ZipScannerDiffblueTest {
  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive2() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "Only file provider resources are supported");

    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>Given {@code Only file provider resources are supported}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_givenOnlyFileProviderResourcesAreSupported() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();

    FileResource src = new FileResource();
    src.setName("Only file provider resources are supported");
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_givenTrue() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();

    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        "Only file provider resources are supported");
    src.setDirectory(true);
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@link BZip2Resource#BZip2Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenBZip2Resource_thenThrowBuildException() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();
    BZip2Resource src = new BZip2Resource();
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenFileResource_thenThrowBuildException() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();
    FileResource src = new FileResource();
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@code r}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenR_thenThrowBuildException() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "r", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenResource_thenThrowBuildException() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();
    Resource src = new Resource();
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenUtf8_thenThrowBuildException() {
    // Arrange
    ZipScanner zipScanner = new ZipScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> zipScanner.fillMapsFromArchive(src, "UTF8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test new {@link ZipScanner} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ZipScanner}
   */
  @Test
  public void testNewZipScanner() {
    // Arrange and Act
    ZipScanner actualZipScanner = new ZipScanner();

    // Assert
    assertNull(actualZipScanner.getBasedir());
    assertNull(actualZipScanner.srcFile);
    assertEquals(0, actualZipScanner.getNotFollowedSymlinks().length);
    assertTrue(actualZipScanner.isCaseSensitive());
    assertTrue(actualZipScanner.isEverythingIncluded());
    assertTrue(actualZipScanner.isFollowSymlinks());
  }
}
