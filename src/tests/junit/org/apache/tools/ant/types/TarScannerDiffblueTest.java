package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.JavaResource;
import org.junit.Test;

public class TarScannerDiffblueTest {
  /**
   * Test {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <p>
   * Method under test: {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> tarScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_givenFileAttributeIsNull() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    FileResource src = new FileResource();
    src.setName("file attribute is null!");
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> tarScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@code Encoding}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenEncoding_thenThrowBuildException() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class, () -> tarScanner.fillMapsFromArchive(src, "Encoding", fileEntries,
        matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenJavaConstantResource_thenThrowBuildException() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    JavaConstantResource src = new JavaConstantResource();
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> tarScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@link JavaResource#JavaResource(String, Path)} with {@code Name} and path is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenJavaResourceWithNameAndPathIsNull() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    JavaResource src = new JavaResource("Name", null);

    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> tarScanner.fillMapsFromArchive(src, "UTF-8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}.
   * <ul>
   *   <li>When {@code UTF8}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link TarScanner#fillMapsFromArchive(Resource, String, Map, Map, Map, Map)}
   */
  @Test
  public void testFillMapsFromArchive_whenUtf8_thenThrowBuildException() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    HashMap<String, Resource> fileEntries = new HashMap<>();
    HashMap<String, Resource> matchFileEntries = new HashMap<>();
    HashMap<String, Resource> dirEntries = new HashMap<>();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> tarScanner.fillMapsFromArchive(src, "UTF8", fileEntries, matchFileEntries, dirEntries, new HashMap<>()));
  }

  /**
   * Test new {@link TarScanner} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link TarScanner}
   */
  @Test
  public void testNewTarScanner() {
    // Arrange and Act
    TarScanner actualTarScanner = new TarScanner();

    // Assert
    assertNull(actualTarScanner.getBasedir());
    assertNull(actualTarScanner.srcFile);
    assertEquals(0, actualTarScanner.getNotFollowedSymlinks().length);
    assertTrue(actualTarScanner.isCaseSensitive());
    assertTrue(actualTarScanner.isEverythingIncluded());
    assertTrue(actualTarScanner.isFollowSymlinks());
  }
}
