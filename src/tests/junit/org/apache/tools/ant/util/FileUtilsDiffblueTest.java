package org.apache.tools.ant.util;

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
import java.nio.channels.Channel;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import org.apache.tools.ant.BuildException;
import org.jruby.util.io.NullChannel;
import org.junit.Ignore;
import org.junit.Test;

public class FileUtilsDiffblueTest {
  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame2() throws IOException {
    // Arrange, Act and Assert
    assertTrue(FileUtils.getFileUtils().areSame(null, null));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame3() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("."), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame4() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(".", "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame5() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get("..", "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame6() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame7() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertFalse(fileUtils.areSame(f1, Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame8() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.areSame(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), null));
  }

  /**
   * Method under test: {@link FileUtils#areSame(File, File)}
   */
  @Test
  public void testAreSame9() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.areSame(null, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#close(Channel)}
   */
  @Test
  public void testClose() {
    // Arrange
    NullChannel nullChannel = new NullChannel();

    // Act
    FileUtils.close(nullChannel);

    // Assert
    assertFalse(nullChannel.isOpen());
  }

  /**
   * Method under test: default or parameterless constructor of {@link FileUtils}
   */
  @Test
  public void testConstructor() {
    // Arrange and Act
    FileUtils actualFileUtils = new FileUtils();

    // Assert
    assertEquals("UTF8", actualFileUtils.getDefaultEncoding());
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, actualFileUtils.getFileTimestampGranularity());
  }

  /**
   * Method under test: {@link FileUtils#contentEquals(File, File)}
   */
  @Test
  public void testContentEquals() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#contentEquals(File, File)}
   */
  @Test
  public void testContentEquals2() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#contentEquals(File, File)}
   */
  @Test
  public void testContentEquals3() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#contentEquals(File, File, boolean)}
   */
  @Test
  public void testContentEquals4() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Method under test: {@link FileUtils#contentEquals(File, File, boolean)}
   */
  @Test
  public void testContentEquals5() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Method under test: {@link FileUtils#contentEquals(File, File, boolean)}
   */
  @Test
  public void testContentEquals6() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(fileUtils.contentEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "").toFile(), true));
  }

  /**
   * Method under test: {@link FileUtils#dissect(String)}
   */
  @Test
  public void testDissect() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().dissect("Path"));
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().dissect(""));
  }

  /**
   * Method under test: {@link FileUtils#fileNameEquals(File, File)}
   */
  @Test
  public void testFileNameEquals() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.fileNameEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#fileNameEquals(File, File)}
   */
  @Test
  public void testFileNameEquals2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(System.getProperty("."), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.fileNameEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#fileNameEquals(File, File)}
   */
  @Test
  public void testFileNameEquals3() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get(".", "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.fileNameEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#fileNameEquals(File, File)}
   */
  @Test
  public void testFileNameEquals4() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File f1 = Paths.get("..", "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.fileNameEquals(f1, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#fromURI(String)}
   */
  @Test
  public void testFromURI() {
    // Arrange, Act and Assert
    assertEquals("", FileUtils.getFileUtils().fromURI("file:"));
    assertEquals("", FileUtils.newFileUtils().fromURI("file:"));
    assertEquals("Uri", FileUtils.getFileUtils().fromURI("file:Uri"));
  }

  /**
   * Method under test: {@link FileUtils#getDefaultEncoding()}
   */
  @Test
  public void testGetDefaultEncoding() {
    // Arrange, Act and Assert
    assertEquals("UTF8", FileUtils.getFileUtils().getDefaultEncoding());
  }

  /**
   * Method under test: {@link FileUtils#getFileTimestampGranularity()}
   */
  @Test
  public void testGetFileTimestampGranularity() {
    // Arrange, Act and Assert
    assertEquals(FileUtils.UNIX_FILE_TIMESTAMP_GRANULARITY, FileUtils.getFileUtils().getFileTimestampGranularity());
  }

  /**
  * Method under test: {@link FileUtils#getFileUtils()}
  */
  @Test
  public void testGetFileUtils() {
    // Arrange and Act
    FileUtils actualFileUtils = FileUtils.getFileUtils();

    // Assert
    assertSame(actualFileUtils, actualFileUtils.getFileUtils());
  }

  /**
   * Method under test: {@link FileUtils#getParentFile(File)}
   */
  @Test
  public void testGetParentFile() {
    // Arrange, Act and Assert
    assertNull(FileUtils.getFileUtils().getParentFile(null));
  }

  /**
   * Method under test: {@link FileUtils#getPath(List)}
   */
  @Test
  public void testGetPath() {
    // Arrange, Act and Assert
    assertEquals("", FileUtils.getPath(new ArrayList<>()));
    assertEquals("", FileUtils.getPath(new ArrayList<>(), 'A'));
  }

  /**
   * Method under test: {@link FileUtils#getPathStack(String)}
   */
  @Test
  public void testGetPathStack() {
    // Arrange and Act
    String[] actualPathStack = FileUtils.getPathStack("Path");

    // Assert
    assertEquals(1, actualPathStack.length);
    assertEquals("Path", actualPathStack[0]);
  }

  /**
   * Method under test: {@link FileUtils#getRelativePath(File, File)}
   */
  @Test
  public void testGetRelativePath() throws Exception {
    // Arrange
    File fromFile = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals("",
        FileUtils.getRelativePath(fromFile, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#getRelativePath(File, File)}
   */
  @Test
  @Ignore
  public void testGetRelativePath2() throws Exception {
    // Arrange
    File fromFile = Paths.get(System.getProperty("/"), "test.txt").toFile();

    // Act
    String actualRelativePath = FileUtils.getRelativePath(fromFile,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals(String.join("", "../../../../../../private",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString()), actualRelativePath);
  }

  /**
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get(".", "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase3() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get("..", "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#hasErrorInCase(File)}
   */
  @Test
  public void testHasErrorInCase4() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.hasErrorInCase(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isAbsolutePath(String)}
   */
  @Test
  public void testIsAbsolutePath() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.isAbsolutePath("foo.txt"));
    assertFalse(FileUtils.isAbsolutePath(""));
  }

  /**
   * Method under test: {@link FileUtils#isCaseSensitiveFileSystem(Path)}
   */
  @Test
  public void testIsCaseSensitiveFileSystem() {
    // Arrange, Act and Assert
    assertFalse(
        FileUtils.isCaseSensitiveFileSystem(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt")).isPresent());
    assertThrows(IllegalArgumentException.class, () -> FileUtils.isCaseSensitiveFileSystem(null));
  }

  /**
   * Method under test: {@link FileUtils#isCaseSensitiveFileSystem(Path)}
   */
  @Test
  public void testIsCaseSensitiveFileSystem2() {
    // Arrange and Act
    Optional<Boolean> actualIsCaseSensitiveFileSystemResult = FileUtils
        .isCaseSensitiveFileSystem(Paths.get(System.getProperty("java.io.tmpdir"), ""));

    // Assert
    assertTrue(actualIsCaseSensitiveFileSystemResult.isPresent());
    assertFalse(actualIsCaseSensitiveFileSystemResult.get());
  }

  /**
   * Method under test: {@link FileUtils#isContextRelativePath(String)}
   */
  @Test
  public void testIsContextRelativePath() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.isContextRelativePath("foo.txt"));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPath() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPath2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("."), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPath3() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(".", "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File)}
   */
  @Test
  public void testIsLeadingPath4() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get("..", "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPath5() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPath6() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("foo"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPath7() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPath8() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("foo"), "test.txt").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPath9() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(".", "test.txt").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Method under test: {@link FileUtils#isLeadingPath(File, File, boolean)}
   */
  @Test
  public void testIsLeadingPath10() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get("..", "test.txt").toFile();

    // Act and Assert
    assertFalse(
        fileUtils.isLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }

  /**
   * Method under test: {@link FileUtils#isSymbolicLink(File, String)}
   */
  @Test
  public void testIsSymbolicLink() throws IOException {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.isSymbolicLink(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), "Name"));
  }

  /**
   * Method under test: {@link FileUtils#isSymbolicLink(File, String)}
   */
  @Test
  public void testIsSymbolicLink2() throws IOException {
    // Arrange, Act and Assert
    assertFalse(FileUtils.getFileUtils().isSymbolicLink(null, "foo"));
  }

  /**
   * Method under test: {@link FileUtils#isUpToDate(long, long)}
   */
  @Test
  public void testIsUpToDate() {
    // Arrange, Act and Assert
    assertFalse(FileUtils.getFileUtils()
        .isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
    assertTrue(FileUtils.getFileUtils().isUpToDate(Long.MAX_VALUE, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
    assertFalse(FileUtils.getFileUtils().isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, -1L));
    assertFalse(FileUtils.getFileUtils()
        .isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY,
            FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
    assertTrue(FileUtils.getFileUtils()
        .isUpToDate(-1L, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
    assertFalse(FileUtils.getFileUtils()
        .isUpToDate(FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY, -1L, FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Method under test: {@link FileUtils#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate3() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isUpToDate(File, File)}
   */
  @Test
  public void testIsUpToDate4() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#isUpToDate(File, File, long)}
   */
  @Test
  public void testIsUpToDate5() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertFalse(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Method under test: {@link FileUtils#isUpToDate(File, File, long)}
   */
  @Test
  public void testIsUpToDate6() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertTrue(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "").toFile(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Method under test: {@link FileUtils#isUpToDate(File, File, long)}
   */
  @Test
  public void testIsUpToDate7() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File source = Paths.get(System.getProperty("java.io.tmpdir"), "").toFile();

    // Act and Assert
    assertFalse(fileUtils.isUpToDate(source, Paths.get(System.getProperty("java.io.tmpdir"), "").toFile(),
        FileUtils.NTFS_FILE_TIMESTAMP_GRANULARITY));
  }

  /**
   * Method under test: {@link FileUtils#normalize(String)}
   */
  @Test
  public void testNormalize() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().normalize("Path"));
    assertThrows(BuildException.class, () -> FileUtils.getFileUtils().normalize(""));
  }

  /**
   * Method under test: {@link FileUtils#readFully(Reader)}
   */
  @Test
  public void testReadFully() throws IOException {
    // Arrange, Act and Assert
    assertEquals("foo", FileUtils.readFully(new StringReader("foo")));
    assertNull(FileUtils.readFully(new StringReader("")));
    assertEquals("foo", FileUtils.readFully(new StringReader("foo"), 3));
    assertThrows(IllegalArgumentException.class, () -> FileUtils.readFully(new StringReader("foo"), 0));
    assertNull(FileUtils.readFully(new StringReader(""), 3));
  }

  /**
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertEquals("",
        fileUtils.removeLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("."), "test.txt").toFile();

    // Act
    String actualRemoveLeadingPathResult = fileUtils.removeLeadingPath(leading,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualRemoveLeadingPathResult);
  }

  /**
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath3() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(".", "test.txt").toFile();

    // Act
    String actualRemoveLeadingPathResult = fileUtils.removeLeadingPath(leading,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualRemoveLeadingPathResult);
  }

  /**
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath4() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get("..", "test.txt").toFile();

    // Act
    String actualRemoveLeadingPathResult = fileUtils.removeLeadingPath(leading,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Assert
    assertEquals(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toString(), actualRemoveLeadingPathResult);
  }

  /**
   * Method under test: {@link FileUtils#removeLeadingPath(File, File)}
   */
  @Test
  public void testRemoveLeadingPath5() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();
    File leading = Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile();

    // Act and Assert
    assertEquals("test.txt",
        fileUtils.removeLeadingPath(leading, Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#safeReadFully(Reader)}
   */
  @Test
  public void testSafeReadFully() throws IOException {
    // Arrange, Act and Assert
    assertEquals("foo", FileUtils.safeReadFully(new StringReader("foo")));
    assertEquals("", FileUtils.safeReadFully(new StringReader("")));
  }

  /**
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
   * Method under test: {@link FileUtils#toVMSPath(File)}
   */
  @Test
  @Ignore
  public void testToVMSPath() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertEquals("var:[folders.x7.nlhtvly92_57d0lhpbzqcd2w0000gp.T]test.txt",
        fileUtils.toVMSPath(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#toVMSPath(File)}
   */
  @Test
  public void testToVMSPath2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    String actualToVMSPathResult = fileUtils.toVMSPath(Paths.get(".", "test.txt").toFile());

    // Assert
    assertEquals(String.join("", "Users:[", System.getProperty("user.name"), ".projects.ant]test.txt"),
        actualToVMSPathResult);
  }

  /**
   * Method under test: {@link FileUtils#toVMSPath(File)}
   */
  @Test
  public void testToVMSPath3() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    String actualToVMSPathResult = fileUtils.toVMSPath(Paths.get("..", "test.txt").toFile());

    // Assert
    assertEquals(String.join("", "Users:[", System.getProperty("user.name"), ".projects]test.txt"),
        actualToVMSPathResult);
  }

  /**
   * Method under test: {@link FileUtils#toVMSPath(File)}
   */
  @Test
  public void testToVMSPath4() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act
    String actualToVMSPathResult = fileUtils.toVMSPath(Paths.get(":", "test.txt").toFile());

    // Assert
    assertEquals(String.join("", "Users:[", System.getProperty("user.name"), ".projects.ant.:]test.txt"),
        actualToVMSPathResult);
  }

  /**
   * Method under test: {@link FileUtils#toVMSPath(File)}
   */
  @Test
  @Ignore
  public void testToVMSPath5() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertEquals("var:[folders.x7.nlhtvly92_57d0lhpbzqcd2w0000gp.T]",
        fileUtils.toVMSPath(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#translatePath(String)}
   */
  @Test
  public void testTranslatePath() {
    // Arrange, Act and Assert
    assertEquals("To Process", FileUtils.translatePath("To Process"));
    assertEquals("", FileUtils.translatePath(null));
    assertEquals("", FileUtils.translatePath(""));
    assertEquals(".", FileUtils.translatePath("."));
    assertEquals("netware:netware", FileUtils.translatePath("netware:;netware"));
  }

  /**
   * Method under test: {@link FileUtils#tryHardToDelete(File)}
   */
  @Test
  public void testTryHardToDelete() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.tryHardToDelete(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Method under test: {@link FileUtils#tryHardToDelete(File, boolean)}
   */
  @Test
  public void testTryHardToDelete2() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.tryHardToDelete(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true));
  }

  /**
   * Method under test: {@link FileUtils#tryHardToDelete(File, boolean)}
   */
  @Test
  public void testTryHardToDelete3() {
    // Arrange
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertFalse(fileUtils.tryHardToDelete(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), false));
  }
}

