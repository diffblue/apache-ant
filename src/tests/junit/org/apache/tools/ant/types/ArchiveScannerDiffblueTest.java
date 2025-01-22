package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class ArchiveScannerDiffblueTest {
  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    FileResource src = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act
    tarScanner.setSrc(src);

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = tarScanner.srcFile;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(File)} with {@code srcFile}.
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(File)}
   */
  @Test
  public void testSetSrcWithSrcFile() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    // Act
    tarScanner.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>Then {@link TarScanner} (default constructor) {@link ArchiveScanner#srcFile} Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc_givenDotDot_thenTarScannerSrcFileNameIsDownloads() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    FileResource src = new FileResource();
    src.setName("..");

    // Act
    tarScanner.setSrc(src);

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("Downloads", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = tarScanner.srcFile;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>Then {@link TarScanner} (default constructor) {@link ArchiveScanner#srcFile} Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc_givenDot_thenTarScannerSrcFileNameIsApacheAnt11015() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    FileResource src = new FileResource();
    src.setName(".");

    // Act
    tarScanner.setSrc(src);

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("apache-ant-1.10.15", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = tarScanner.srcFile;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc_givenDot_whenFileResourceNameIsDot() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    FileResource src = new FileResource();
    src.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    src.setName(".");

    // Act
    tarScanner.setSrc(src);

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = tarScanner.srcFile;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    FileResource src = new FileResource();
    src.setName("");

    // Act
    tarScanner.setSrc(src);

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("apache-ant-1.10.15", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = tarScanner.srcFile;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>Then {@link TarScanner} (default constructor) {@link ArchiveScanner#srcFile} Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc_givenName_thenTarScannerSrcFileNameIsName() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    FileResource src = new FileResource();
    src.setName("Name");

    // Act
    tarScanner.setSrc(src);

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("Name", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = tarScanner.srcFile;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <ul>
   *   <li>Given Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toString.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc_givenPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToString() {
    // Arrange
    TarScanner tarScanner = new TarScanner();

    FileResource src = new FileResource();
    src.setBaseDir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    src.setName(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString());

    // Act
    tarScanner.setSrc(src);

    // Assert
    File file = tarScanner.srcFile;
    assertEquals("test.txt", file.getName());
    assertTrue(file.isAbsolute());
    File expectedFile = tarScanner.srcFile;
    assertSame(expectedFile, src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#setSrc(Resource)} with {@code src}.
   * <ul>
   *   <li>When {@link FileResource#FileResource()}.</li>
   *   <li>Then {@link FileResource#FileResource()} File is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#setSrc(Resource)}
   */
  @Test
  public void testSetSrcWithSrc_whenFileResource_thenFileResourceFileIsNull() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    FileResource src = new FileResource();

    // Act
    tarScanner.setSrc(src);

    // Assert that nothing has changed
    assertNull(src.getFile());
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{"?", "?"});

    // Act and Assert
    assertFalse(tarScanner.match("Path"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath2() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{"**", "?"});

    // Act and Assert
    assertTrue(tarScanner.match(""));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath3() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{"", "?"});

    // Act and Assert
    assertTrue(tarScanner.match(""));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addDefaultExcludes.</li>
   *   <li>When {@code .git}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddDefaultExcludes_whenGit_thenReturnFalse() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addDefaultExcludes();
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(tarScanner.match(".git"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addDefaultExcludes.</li>
   *   <li>When {@code Path}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddDefaultExcludes_whenPath_thenReturnTrue() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addDefaultExcludes();
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(tarScanner.match("Path"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addExcludes array of {@link String} with {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddExcludesArrayOfStringWithAsterisk() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(tarScanner.match("Path"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addExcludes array of {@link String} with {@code **}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddExcludesArrayOfStringWithAsteriskAsterisk() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"**"});
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(tarScanner.match("Path"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addExcludes array of {@link String} with {@code *}.</li>
   *   <li>When {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddExcludesArrayOfStringWithAsterisk_whenSlash() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(tarScanner.match("/"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addExcludes array of {@link String} with empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddExcludesArrayOfStringWithEmptyString() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{""});
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{"**", "?"});

    // Act and Assert
    assertFalse(tarScanner.match(""));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addExcludes array of {@link String} with {@code Excludes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddExcludesArrayOfStringWithExcludes() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"Excludes"});
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(tarScanner.match("Path"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) addExcludes array of {@link String} with {@code ?}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerAddExcludesArrayOfStringWithQuestionMark() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"?"});
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertTrue(tarScanner.match("Path"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>Given {@link TarScanner} (default constructor) Includes is empty array of {@link String}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_givenTarScannerIncludesIsEmptyArrayOfString_thenReturnFalse() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{});

    // Act and Assert
    assertFalse(tarScanner.match("Path"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>When {@code *}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_whenAsterisk() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"?"});
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(tarScanner.match("*"));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_whenEmptyString() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addExcludes(new String[]{"*"});
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(tarScanner.match(""));
  }

  /**
   * Test {@link ArchiveScanner#match(String)} with {@code path}.
   * <ul>
   *   <li>When {@code %*%}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#match(String)}
   */
  @Test
  public void testMatchWithPath_whenPercentSignAsteriskPercentSign() {
    // Arrange
    TarScanner tarScanner = new TarScanner();
    tarScanner.addDefaultExcludes();
    tarScanner.setIncludes(new String[]{"*", "?"});

    // Act and Assert
    assertFalse(tarScanner.match("%*%"));
  }

  /**
   * Test {@link ArchiveScanner#trimSeparator(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#trimSeparator(String)}
   */
  @Test
  public void testTrimSeparator_whenFoo_thenReturnFoo() {
    // Arrange, Act and Assert
    assertEquals("foo", ArchiveScanner.trimSeparator("foo"));
  }

  /**
   * Test {@link ArchiveScanner#trimSeparator(String)}.
   * <ul>
   *   <li>When {@code /}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ArchiveScanner#trimSeparator(String)}
   */
  @Test
  public void testTrimSeparator_whenSlash_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", ArchiveScanner.trimSeparator("/"));
  }
}
