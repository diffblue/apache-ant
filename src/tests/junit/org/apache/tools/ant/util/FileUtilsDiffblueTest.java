package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.io.Reader;
import java.io.StringReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class FileUtilsDiffblueTest {
  /**
   * Test {@link FileUtils#newFileUtils()}.
   * <p>
   * Method under test: {@link FileUtils#newFileUtils()}
   */
  @Test
  public void testNewFileUtils2() {
    // Arrange and Act
    FileUtils actualNewFileUtilsResult = FileUtils.newFileUtils();

    // Assert
    assertEquals("UTF8", actualNewFileUtilsResult.getDefaultEncoding());
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, actualNewFileUtilsResult.getFileTimestampGranularity());
  }

  /**
   * Test getters and setters.
   * <p>
   * Method under test: {@link FileUtils#getFileUtils()}
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    FileUtils actualFileUtils = FileUtils.getFileUtils();

    // Assert
    assertSame(actualFileUtils, actualFileUtils.getFileUtils());
  }

  /**
   * Test {@link FileUtils#FileUtils()}.
   * <p>
   * Method under test: default or parameterless constructor of {@link FileUtils}
   */
  @Test
  public void testNewFileUtils() {
    // Arrange and Act
    FileUtils actualFileUtils = new FileUtils();

    // Assert
    assertEquals("UTF8", actualFileUtils.getDefaultEncoding());
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, actualFileUtils.getFileTimestampGranularity());
  }

  /**
   * Test {@link FileUtils#getFileURL(File)}.
   * <p>
   * Method under test: {@link FileUtils#getFileURL(File)}
   */
  @Test
  public void testGetFileURL() throws MalformedURLException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    URL actualFileURL = fileUtils.getFileURL(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    String expectedToStringResult = String.join("", "file:",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString().concat(File.separator));
    assertEquals(expectedToStringResult, actualFileURL.toString());
  }

  /**
   * Test {@link FileUtils#resolveFile(File, String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#resolveFile(File, String)}
   */
  @Test
  public void testResolveFile_whenEmptyString_thenReturnNameIsTestTxt() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    File actualResolveFileResult = fileUtils
        .resolveFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "");

    // Assert
    assertEquals("test.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link FileUtils#resolveFile(File, String)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   *   <li>Then return Name is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#resolveFile(File, String)}
   */
  @Test
  public void testResolveFile_whenPropertyIsJavaIoTmpdirIsDotDotToFile_thenReturnNameIsFooTxt() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    File actualResolveFileResult = fileUtils.resolveFile(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile(),
        "foo.txt");

    // Assert
    assertEquals("foo.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link FileUtils#resolveFile(File, String)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   *   <li>Then return Name is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#resolveFile(File, String)}
   */
  @Test
  public void testResolveFile_whenPropertyIsJavaIoTmpdirIsDotToFile_thenReturnNameIsFooTxt() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    File actualResolveFileResult = fileUtils.resolveFile(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile(),
        "foo.txt");

    // Assert
    assertEquals("foo.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link FileUtils#resolveFile(File, String)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return Name is {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#resolveFile(File, String)}
   */
  @Test
  public void testResolveFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnNameIsFooTxt() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    File actualResolveFileResult = fileUtils
        .resolveFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "foo.txt");

    // Assert
    assertEquals("foo.txt", actualResolveFileResult.getName());
    assertTrue(actualResolveFileResult.isAbsolute());
  }

  /**
   * Test {@link FileUtils#isContextRelativePath(String)}.
   * <p>
   * Method under test: {@link FileUtils#isContextRelativePath(String)}
   */
  @Test
  public void testIsContextRelativePath() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.isContextRelativePath("foo.txt"));
  }

  /**
   * Test {@link FileUtils#isAbsolutePath(String)}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isAbsolutePath(String)}
   */
  @Test
  public void testIsAbsolutePath_whenEmptyString() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.isAbsolutePath(""));
  }

  /**
   * Test {@link FileUtils#isAbsolutePath(String)}.
   * <ul>
   *   <li>When {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isAbsolutePath(String)}
   */
  @Test
  public void testIsAbsolutePath_whenFooTxt() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.isAbsolutePath("foo.txt"));
  }

  /**
   * Test {@link FileUtils#translatePath(String)}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then return {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenDot_thenReturnDot() {
    // Arrange, Act and Assert
    assertEquals(".", FileUtils.translatePath("."));
  }

  /**
   * Test {@link FileUtils#translatePath(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenEmptyString_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", FileUtils.translatePath(""));
  }

  /**
   * Test {@link FileUtils#translatePath(String)}.
   * <ul>
   *   <li>When {@code netware:;netware}.</li>
   *   <li>Then return {@code netware:netware}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenNetwareNetware_thenReturnNetwareNetware() {
    // Arrange, Act and Assert
    assertEquals("netware:netware", FileUtils.translatePath("netware:;netware"));
  }

  /**
   * Test {@link FileUtils#translatePath(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenNull_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", FileUtils.translatePath(null));
  }

  /**
   * Test {@link FileUtils#translatePath(String)}.
   * <ul>
   *   <li>When {@code To Process}.</li>
   *   <li>Then return {@code To Process}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#translatePath(String)}
   */
  @Test
  public void testTranslatePath_whenToProcess_thenReturnToProcess() {
    // Arrange, Act and Assert
    assertEquals("To Process", FileUtils.translatePath("To Process"));
  }

  /**
   * Test {@link FileUtils#normalize(String)}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#normalize(String)}
   */
  @Test
  public void testNormalize_whenEmptyString() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().normalize(""));
  }

  /**
   * Test {@link FileUtils#normalize(String)}.
   * <ul>
   *   <li>When {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#normalize(String)}
   */
  @Test
  public void testNormalize_whenPath() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().normalize("Path"));
  }

  /**
   * Test {@link FileUtils#dissect(String)}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#dissect(String)}
   */
  @Test
  public void testDissect_whenEmptyString() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().dissect(""));
  }

  /**
   * Test {@link FileUtils#dissect(String)}.
   * <ul>
   *   <li>When {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#dissect(String)}
   */
  @Test
  public void testDissect_whenPath() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().dissect("Path"));
  }

  /**
   * Test {@link FileUtils#createTempFile(String, String, File)} with {@code prefix}, {@code suffix}, {@code parentDir}.
   * <p>
   * Method under test: {@link FileUtils#createTempFile(String, String, File)}
   */
  @Test
  public void testCreateTempFileWithPrefixSuffixParentDir() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertTrue(fileUtils
        .createTempFile("Prefix", "Suffix", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())
        .isAbsolute());
  }

  /**
   * Test {@link FileUtils#createTempFile(String, String, File, boolean)} with {@code prefix}, {@code suffix}, {@code parentDir}, {@code deleteOnExit}.
   * <p>
   * Method under test: {@link FileUtils#createTempFile(String, String, File, boolean)}
   */
  @Test
  public void testCreateTempFileWithPrefixSuffixParentDirDeleteOnExit() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertTrue(fileUtils
        .createTempFile("Prefix", "Suffix", Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true)
        .isAbsolute());
  }

  /**
   * Test {@link FileUtils#createTempFile(String, String, File, boolean, boolean)} with {@code prefix}, {@code suffix}, {@code parentDir}, {@code deleteOnExit}, {@code createFile}.
   * <p>
   * Method under test: {@link FileUtils#createTempFile(String, String, File, boolean, boolean)}
   */
  @Test
  public void testCreateTempFileWithPrefixSuffixParentDirDeleteOnExitCreateFile() {
    // Arrange, Act and Assert
    assertTrue(FileUtils.getFileUtils().createTempFile("Prefix", "Suffix", null, false, false).isAbsolute());
  }

  /**
   * Test {@link FileUtils#createTempFile(String, String, File, boolean)} with {@code prefix}, {@code suffix}, {@code parentDir}, {@code deleteOnExit}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#createTempFile(String, String, File, boolean)}
   */
  @Test
  public void testCreateTempFileWithPrefixSuffixParentDirDeleteOnExit_whenNull() {
    // Arrange, Act and Assert
    assertTrue(FileUtils.getFileUtils().createTempFile("Prefix", "Suffix", null, false).isAbsolute());
  }

  /**
   * Test {@link FileUtils#createTempFile(String, String, File)} with {@code prefix}, {@code suffix}, {@code parentDir}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#createTempFile(String, String, File)}
   */
  @Test
  public void testCreateTempFileWithPrefixSuffixParentDir_whenNull() {
    // Arrange, Act and Assert
    assertTrue(FileUtils.getFileUtils().createTempFile("Prefix", "Suffix", null).isAbsolute());
  }

  /**
   * Test {@link FileUtils#createTempFile(Project, String, String, File, boolean, boolean)} with {@code project}, {@code prefix}, {@code suffix}, {@code parentDir}, {@code deleteOnExit}, {@code createFile}.
   * <p>
   * Method under test: {@link FileUtils#createTempFile(Project, String, String, File, boolean, boolean)}
   */
  @Test
  public void testCreateTempFileWithProjectPrefixSuffixParentDirDeleteOnExitCreateFile() {
    // Arrange, Act and Assert
    assertTrue(FileUtils.getFileUtils().createTempFile(null, "Prefix", "Suffix", null, false, false).isAbsolute());
  }

  /**
   * Test {@link FileUtils#createTempFile(Project, String, String, File, boolean, boolean)} with {@code project}, {@code prefix}, {@code suffix}, {@code parentDir}, {@code deleteOnExit}, {@code createFile}.
   * <p>
   * Method under test: {@link FileUtils#createTempFile(Project, String, String, File, boolean, boolean)}
   */
  @Test
  public void testCreateTempFileWithProjectPrefixSuffixParentDirDeleteOnExitCreateFile2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertTrue(fileUtils.createTempFile(new Project(), "Prefix", "Suffix", null, false, false).isAbsolute());
  }

  /**
   * Test {@link FileUtils#contentEquals(File, File, boolean)} with {@code f1}, {@code f2}, {@code textfile}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#contentEquals(File, File, boolean)}
   */
  @Test
  public void testContentEqualsWithF1F2Textfile_whenPropertyIsJavaIoTmpdirIs42And42ToFile() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Test {@link FileUtils#contentEquals(File, File, boolean)} with {@code f1}, {@code f2}, {@code textfile}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#contentEquals(File, File, boolean)}
   */
  @Test
  public void testContentEqualsWithF1F2Textfile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Test {@link FileUtils#contentEquals(File, File)} with {@code f1}, {@code f2}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#contentEquals(File, File)}
   */
  @Test
  public void testContentEqualsWithF1F2_whenPropertyIsJavaIoTmpdirIs42And42ToFile() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile();

    // Act and Assert
    assertFalse(fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#contentEquals(File, File)} with {@code f1}, {@code f2}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#contentEquals(File, File)}
   */
  @Test
  public void testContentEqualsWithF1F2_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#getParentFile(File)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getParentFile(File)}
   */
  @Test
  public void testGetParentFile_whenNull_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull(FileUtils.getFileUtils().getParentFile(null));
  }

  /**
   * Test {@link FileUtils#readFully(Reader, int)} with {@code rdr}, {@code bufferSize}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#readFully(Reader, int)}
   */
  @Test
  public void testReadFullyWithRdrBufferSize_whenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull(FileUtils.readFully(new StringReader(""), 3));
  }

  /**
   * Test {@link FileUtils#readFully(Reader, int)} with {@code rdr}, {@code bufferSize}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#readFully(Reader, int)}
   */
  @Test
  public void testReadFullyWithRdrBufferSize_whenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange, Act and Assert
    assertEquals("foo", FileUtils.readFully(new StringReader("foo"), 3));
  }

  /**
   * Test {@link FileUtils#readFully(Reader, int)} with {@code rdr}, {@code bufferSize}.
   * <ul>
   *   <li>When zero.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#readFully(Reader, int)}
   */
  @Test
  public void testReadFullyWithRdrBufferSize_whenZero_thenThrowIllegalArgumentException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> FileUtils.readFully(new StringReader("foo"), 0));
  }

  /**
   * Test {@link FileUtils#readFully(Reader)} with {@code rdr}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#readFully(Reader)}
   */
  @Test
  public void testReadFullyWithRdr_whenStringReaderWithEmptyString_thenReturnNull() throws IOException {
    // Arrange, Act and Assert
    assertNull(FileUtils.readFully(new StringReader("")));
  }

  /**
   * Test {@link FileUtils#readFully(Reader)} with {@code rdr}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#readFully(Reader)}
   */
  @Test
  public void testReadFullyWithRdr_whenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange, Act and Assert
    assertEquals("foo", FileUtils.readFully(new StringReader("foo")));
  }

  /**
   * Test {@link FileUtils#safeReadFully(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#safeReadFully(Reader)}
   */
  @Test
  public void testSafeReadFully_whenStringReaderWithEmptyString_thenReturnEmptyString() throws IOException {
    // Arrange, Act and Assert
    assertEquals("", FileUtils.safeReadFully(new StringReader("")));
  }

  /**
   * Test {@link FileUtils#safeReadFully(Reader)}.
   * <ul>
   *   <li>When {@link StringReader#StringReader(String)} with {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#safeReadFully(Reader)}
   */
  @Test
  public void testSafeReadFully_whenStringReaderWithFoo_thenReturnFoo() throws IOException {
    // Arrange, Act and Assert
    assertEquals("foo", FileUtils.safeReadFully(new StringReader("foo")));
  }

  /**
   * Test {@link FileUtils#isSymbolicLink(File, String)}.
   * <ul>
   *   <li>When {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isSymbolicLink(File, String)}
   */
  @Test
  public void testIsSymbolicLink_whenName_thenReturnFalse() throws IOException {
    // Arrange, Act and Assert
    assertFalse(FileUtils.getFileUtils().isSymbolicLink(null, "Name"));
  }

  /**
   * Test {@link FileUtils#isSymbolicLink(File, String)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isSymbolicLink(File, String)}
   */
  @Test
  public void testIsSymbolicLink_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnFalse() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.isSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Name"));
  }

  /**
   * Test {@link FileUtils#removeLeadingPath(File, File)}.
   * <p>
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "var").toFile();

    // Act
    String actualRemoveLeadingPathResult = fileUtils.removeLeadingPath(leading,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualRemoveLeadingPathResult);
  }

  /**
   * Test {@link FileUtils#removeLeadingPath(File, File)}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath_thenReturnEmptyString() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals("",
        fileUtils.removeLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#removeLeadingPath(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   *   <li>Then return {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath_whenPropertyIsJavaIoTmpdirIsDotToFile_thenReturnTestTxt() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertEquals("test.txt",
        fileUtils.removeLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File, boolean)} with {@code leading}, {@code path}, {@code resolveSymlinks}.
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPathResolveSymlinks() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertTrue(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File, boolean)} with {@code leading}, {@code path}, {@code resolveSymlinks}.
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPathResolveSymlinks2() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile();

    // Act and Assert
    assertTrue(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File, boolean)} with {@code leading}, {@code path}, {@code resolveSymlinks}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPathResolveSymlinks_thenReturnFalse() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File, boolean)} with {@code leading}, {@code path}, {@code resolveSymlinks}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPathResolveSymlinks_thenReturnFalse2() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File, boolean)} with {@code leading}, {@code path}, {@code resolveSymlinks}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPathResolveSymlinks_whenFalse_thenReturnTrue() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File, boolean)} with {@code leading}, {@code path}, {@code resolveSymlinks}.
   * <ul>
   *   <li>When {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPathResolveSymlinks_whenTrue_thenReturnTrue() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File)} with {@code leading}, {@code path}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPath_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "var").toFile();

    // Act and Assert
    assertFalse(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File)} with {@code leading}, {@code path}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPath_thenReturnTrue() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File)} with {@code leading}, {@code path}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPath_whenPropertyIsJavaIoTmpdirIsDotDotToFile() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile();

    // Act and Assert
    assertTrue(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#isLeadingPath(File, File)} with {@code leading}, {@code path}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPathWithLeadingPath_whenPropertyIsJavaIoTmpdirIsDotToFile() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertTrue(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#toURI(String)}.
   * <p>
   * Method under test: {@link FileUtils#toURI(String)}
   */
  @Test
  public void testToURI() {
    // Arrange and Act
    String actualToURIResult = FileUtils.getFileUtils().toURI("Path");

    // Assert
    assertEquals(String.join("", "file:", Paths.get(System.getProperty("user.dir"), "Path").toString()),
        actualToURIResult);
  }

  /**
   * Test {@link FileUtils#fileNameEquals(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#fileNameEquals(File, File)}
   */
  @Test
  public void testFileNameEquals_whenPropertyIsJavaIoTmpdirIsDotDotToFile_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile();

    // Act and Assert
    assertFalse(fileUtils.fileNameEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#fileNameEquals(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#fileNameEquals(File, File)}
   */
  @Test
  public void testFileNameEquals_whenPropertyIsJavaIoTmpdirIsDotToFile_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertFalse(fileUtils.fileNameEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#fileNameEquals(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#fileNameEquals(File, File)}
   */
  @Test
  public void testFileNameEquals_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnTrue() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.fileNameEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#areSame(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame_whenPropertyIsJavaIoTmpdirIsDotDotToFile_thenReturnFalse() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#areSame(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame_whenPropertyIsJavaIoTmpdirIsDotToFile_thenReturnFalse() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#areSame(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnTrue() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#areSame(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code var} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame_whenPropertyIsJavaIoTmpdirIsVarToFile_thenReturnFalse() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "var").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#areSame(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code var} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame_whenPropertyIsJavaIoTmpdirIsVarToFile_thenReturnFalse2() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "var").toFile()));
  }

  /**
   * Test {@link FileUtils#getFileTimestampGranularity()}.
   * <p>
   * Method under test: {@link FileUtils#getFileTimestampGranularity()}
   */
  @Test
  public void testGetFileTimestampGranularity() {
    // Arrange, Act and Assert
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, FileUtils.getFileUtils().getFileTimestampGranularity());
  }

  /**
   * Test {@link FileUtils#hasErrorInCase(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase_whenPropertyIsJavaIoTmpdirIsDotDotToFile() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile()));
  }

  /**
   * Test {@link FileUtils#hasErrorInCase(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase_whenPropertyIsJavaIoTmpdirIsDotToFile() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));
  }

  /**
   * Test {@link FileUtils#hasErrorInCase(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#hasErrorInCase(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code var} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase_whenPropertyIsJavaIoTmpdirIsVarToFile() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get(System.getProperty("java.io.tmpdir"), "var").toFile()));
  }

  /**
   * Test {@link FileUtils#isUpToDate(File, File, long)} with {@code source}, {@code dest}, {@code granularity}.
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(File, File, long)}
   */
  @Test
  public void testIsUpToDateWithSourceDestGranularity() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile(),
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(File, File, long)} with {@code source}, {@code dest}, {@code granularity}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(File, File, long)}
   */
  @Test
  public void testIsUpToDateWithSourceDestGranularity_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(File, File, long)} with {@code source}, {@code dest}, {@code granularity}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(File, File, long)}
   */
  @Test
  public void testIsUpToDateWithSourceDestGranularity_thenReturnTrue() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "foo").toFile();

    // Act and Assert
    assertTrue(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(File, File)} with {@code source}, {@code dest}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDateWithSourceDest_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#isUpToDate(File, File)} with {@code source}, {@code dest}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDateWithSourceDest_thenReturnTrue() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "foo", "42").toFile();

    // Act and Assert
    assertTrue(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#isUpToDate(File, File)} with {@code source}, {@code dest}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code 42} and {@code 42} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDateWithSourceDest_whenPropertyIsJavaIoTmpdirIs42And42ToFile() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test {@link FileUtils#isUpToDate(long, long, long)} with {@code sourceTime}, {@code destTime}, {@code granularity}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(long, long, long)}
   */
  @Test
  public void testIsUpToDateWithSourceTimeDestTimeGranularity_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.getFileUtils()
        .isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY,
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(long, long, long)} with {@code sourceTime}, {@code destTime}, {@code granularity}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(long, long, long)}
   */
  @Test
  public void testIsUpToDateWithSourceTimeDestTimeGranularity_whenMinusOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.getFileUtils()
        .isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, -1L, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(long, long, long)} with {@code sourceTime}, {@code destTime}, {@code granularity}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(long, long, long)}
   */
  @Test
  public void testIsUpToDateWithSourceTimeDestTimeGranularity_whenMinusOne_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(FileUtils.getFileUtils()
        .isUpToDate(-1L, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(long, long)} with {@code sourceTime}, {@code destTime}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(long, long)}
   */
  @Test
  public void testIsUpToDateWithSourceTimeDestTime_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.getFileUtils()
        .isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(long, long)} with {@code sourceTime}, {@code destTime}.
   * <ul>
   *   <li>When {@link Long#MAX_VALUE}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(long, long)}
   */
  @Test
  public void testIsUpToDateWithSourceTimeDestTime_whenMax_value_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(FileUtils.getFileUtils().isUpToDate(Long.MAX_VALUE, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Test {@link FileUtils#isUpToDate(long, long)} with {@code sourceTime}, {@code destTime}.
   * <ul>
   *   <li>When minus one.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isUpToDate(long, long)}
   */
  @Test
  public void testIsUpToDateWithSourceTimeDestTime_whenMinusOne_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.getFileUtils().isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, -1L));
  }

  /**
   * Test {@link FileUtils#tryHardToDelete(File, boolean)} with {@code f}, {@code runGC}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#tryHardToDelete(File, boolean)}
   */
  @Test
  public void testTryHardToDeleteWithFRunGC_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(
        fileUtils.tryHardToDelete(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile(), true));
  }

  /**
   * Test {@link FileUtils#tryHardToDelete(File, boolean)} with {@code f}, {@code runGC}.
   * <ul>
   *   <li>When {@code false}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#tryHardToDelete(File, boolean)}
   */
  @Test
  public void testTryHardToDeleteWithFRunGC_whenFalse_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(
        fileUtils.tryHardToDelete(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile(), false));
  }

  /**
   * Test {@link FileUtils#tryHardToDelete(File)} with {@code f}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#tryHardToDelete(File)}
   */
  @Test
  public void testTryHardToDeleteWithF_thenReturnFalse() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.tryHardToDelete(Paths.get(System.getProperty("java.io.tmpdir"), "42", "42", "foo").toFile()));
  }

  /**
   * Test {@link FileUtils#getRelativePath(File, File)}.
   * <ul>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getRelativePath(File, File)}
   */
  @Test
  public void testGetRelativePath_thenReturnEmptyString() throws Exception {
    // Arrange
    File fromFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals("",
        FileUtils.getRelativePath(fromFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#getRelativePath(File, File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code private} toFile.</li>
   *   <li>Then return {@code ../test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getRelativePath(File, File)}
   */
  @Test
  public void testGetRelativePath_whenPropertyIsJavaIoTmpdirIsPrivateToFile_thenReturnTestTxt() throws Exception {
    // Arrange
    File fromFile = Paths.get(System.getProperty("java.io.tmpdir"), "private").toFile();

    // Act and Assert
    assertEquals("../test.txt",
        FileUtils.getRelativePath(fromFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link FileUtils#getPathStack(String)}.
   * <p>
   * Method under test: {@link FileUtils#getPathStack(String)}
   */
  @Test
  public void testGetPathStack() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"Path"}, FileUtils.getPathStack("Path"));
  }

  /**
   * Test {@link FileUtils#getPath(List, char)} with {@code pathStack}, {@code separatorChar}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>Then return {@code 42Afoo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getPath(List, char)}
   */
  @Test
  public void testGetPathWithPathStackSeparatorChar_given42_thenReturn42Afoo() {
    // Arrange
    ArrayList<String> pathStack = new ArrayList<>();
    pathStack.add("42");
    pathStack.add("foo");

    // Act and Assert
    assertEquals("42Afoo", FileUtils.getPath(pathStack, 'A'));
  }

  /**
   * Test {@link FileUtils#getPath(List, char)} with {@code pathStack}, {@code separatorChar}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getPath(List, char)}
   */
  @Test
  public void testGetPathWithPathStackSeparatorChar_givenFoo_whenArrayListAddFoo_thenReturnFoo() {
    // Arrange
    ArrayList<String> pathStack = new ArrayList<>();
    pathStack.add("foo");

    // Act and Assert
    assertEquals("foo", FileUtils.getPath(pathStack, 'A'));
  }

  /**
   * Test {@link FileUtils#getPath(List, char)} with {@code pathStack}, {@code separatorChar}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getPath(List, char)}
   */
  @Test
  public void testGetPathWithPathStackSeparatorChar_whenArrayList_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", FileUtils.getPath(new ArrayList<>(), 'A'));
  }

  /**
   * Test {@link FileUtils#getPath(List)} with {@code pathStack}.
   * <ul>
   *   <li>Given {@code 42}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code 42}.</li>
   *   <li>Then return {@code 42/foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getPath(List)}
   */
  @Test
  public void testGetPathWithPathStack_given42_whenArrayListAdd42_thenReturn42Foo() {
    // Arrange
    ArrayList<String> pathStack = new ArrayList<>();
    pathStack.add("42");
    pathStack.add("foo");

    // Act and Assert
    assertEquals("42/foo", FileUtils.getPath(pathStack));
  }

  /**
   * Test {@link FileUtils#getPath(List)} with {@code pathStack}.
   * <ul>
   *   <li>Given {@code foo}.</li>
   *   <li>When {@link ArrayList#ArrayList()} add {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getPath(List)}
   */
  @Test
  public void testGetPathWithPathStack_givenFoo_whenArrayListAddFoo_thenReturnFoo() {
    // Arrange
    ArrayList<String> pathStack = new ArrayList<>();
    pathStack.add("foo");

    // Act and Assert
    assertEquals("foo", FileUtils.getPath(pathStack));
  }

  /**
   * Test {@link FileUtils#getPath(List)} with {@code pathStack}.
   * <ul>
   *   <li>When {@link ArrayList#ArrayList()}.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#getPath(List)}
   */
  @Test
  public void testGetPathWithPathStack_whenArrayList_thenReturnEmptyString() {
    // Arrange, Act and Assert
    assertEquals("", FileUtils.getPath(new ArrayList<>()));
  }

  /**
   * Test {@link FileUtils#getDefaultEncoding()}.
   * <p>
   * Method under test: {@link FileUtils#getDefaultEncoding()}
   */
  @Test
  public void testGetDefaultEncoding() {
    // Arrange, Act and Assert
    assertEquals("UTF8", FileUtils.getFileUtils().getDefaultEncoding());
  }

  /**
   * Test {@link FileUtils#isCaseSensitiveFileSystem(Path)}.
   * <ul>
   *   <li>Then return not {@link Optional#get()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isCaseSensitiveFileSystem(Path)}
   */
  @Test
  public void testIsCaseSensitiveFileSystem_thenReturnNotGet() {
    // Arrange and Act
    Optional<Boolean> actualIsCaseSensitiveFileSystemResult = FileUtils
        .isCaseSensitiveFileSystem(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt"));

    // Assert
    assertFalse(actualIsCaseSensitiveFileSystemResult.get());
    assertTrue(actualIsCaseSensitiveFileSystemResult.isPresent());
  }

  /**
   * Test {@link FileUtils#isCaseSensitiveFileSystem(Path)}.
   * <ul>
   *   <li>Then return not Present.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isCaseSensitiveFileSystem(Path)}
   */
  @Test
  public void testIsCaseSensitiveFileSystem_thenReturnNotPresent() {
    // Arrange and Act
    Optional<Boolean> actualIsCaseSensitiveFileSystemResult = FileUtils
        .isCaseSensitiveFileSystem(Paths.get(System.getProperty("java.io.tmpdir"), "aNt"));

    // Assert
    assertFalse(actualIsCaseSensitiveFileSystemResult.isPresent());
  }

  /**
   * Test {@link FileUtils#isCaseSensitiveFileSystem(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link IllegalArgumentException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileUtils#isCaseSensitiveFileSystem(Path)}
   */
  @Test
  public void testIsCaseSensitiveFileSystem_whenNull_thenThrowIllegalArgumentException() {
    // Arrange, Act and Assert
    assertThrows(IllegalArgumentException.class, () -> FileUtils.isCaseSensitiveFileSystem(null));
  }
}
