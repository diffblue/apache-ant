package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.types.optional.ScriptSelector;
import org.junit.Test;

public class ZipFileSetDiffblueTest {
  /**
   * Test {@link ZipFileSet#ZipFileSet()}.
   * <p>
   * Method under test: {@link ZipFileSet#ZipFileSet()}
   */
  @Test
  public void testNewZipFileSet() {
    // Arrange and Act
    ZipFileSet actualZipFileSet = new ZipFileSet();

    // Assert
    assertEquals("", actualZipFileSet.getFullpath());
    assertEquals("", actualZipFileSet.getPrefix());
    assertEquals("ZipFileSet", actualZipFileSet.getDataTypeName());
    assertNull(actualZipFileSet.getDir());
    assertNull(actualZipFileSet.getSrc());
    assertNull(actualZipFileSet.getDescription());
    assertNull(actualZipFileSet.getEncoding());
    assertNull(actualZipFileSet.getProject());
    assertNull(actualZipFileSet.getRefid());
    assertEquals(5, actualZipFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualZipFileSet.isReference());
    assertTrue(actualZipFileSet.getDefaultexcludes());
    assertTrue(actualZipFileSet.getErrorOnMissingDir());
    assertTrue(actualZipFileSet.isFilesystemOnly());
    assertTrue(actualZipFileSet.isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, actualZipFileSet.getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, actualZipFileSet.getFileMode());
  }

  /**
   * Test {@link ZipFileSet#ZipFileSet(FileSet)}.
   * <ul>
   *   <li>When {@link FileSet#FileSet()}.</li>
   *   <li>Then return Fullpath is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipFileSet#ZipFileSet(FileSet)}
   */
  @Test
  public void testNewZipFileSet_whenFileSet_thenReturnFullpathIsEmptyString() {
    // Arrange and Act
    ZipFileSet actualZipFileSet = new ZipFileSet(new FileSet());

    // Assert
    assertEquals("", actualZipFileSet.getFullpath());
    assertEquals("", actualZipFileSet.getPrefix());
    assertEquals("ZipFileSet", actualZipFileSet.getDataTypeName());
    assertNull(actualZipFileSet.getDir());
    assertNull(actualZipFileSet.getSrc());
    assertNull(actualZipFileSet.getDescription());
    assertNull(actualZipFileSet.getEncoding());
    assertNull(actualZipFileSet.getProject());
    assertNull(actualZipFileSet.getRefid());
    assertEquals(5, actualZipFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualZipFileSet.isReference());
    assertTrue(actualZipFileSet.getDefaultexcludes());
    assertTrue(actualZipFileSet.getErrorOnMissingDir());
    assertTrue(actualZipFileSet.isFilesystemOnly());
    assertTrue(actualZipFileSet.isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, actualZipFileSet.getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, actualZipFileSet.getFileMode());
  }

  /**
   * Test {@link ZipFileSet#ZipFileSet(ZipFileSet)}.
   * <ul>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then return Fullpath is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipFileSet#ZipFileSet(ZipFileSet)}
   */
  @Test
  public void testNewZipFileSet_whenZipFileSet_thenReturnFullpathIsEmptyString() {
    // Arrange and Act
    ZipFileSet actualZipFileSet = new ZipFileSet(new ZipFileSet());

    // Assert
    assertEquals("", actualZipFileSet.getFullpath());
    assertEquals("", actualZipFileSet.getPrefix());
    assertEquals("ZipFileSet", actualZipFileSet.getDataTypeName());
    assertNull(actualZipFileSet.getDir());
    assertNull(actualZipFileSet.getSrc());
    assertNull(actualZipFileSet.getDescription());
    assertNull(actualZipFileSet.getEncoding());
    assertNull(actualZipFileSet.getProject());
    assertNull(actualZipFileSet.getRefid());
    assertEquals(5, actualZipFileSet.getMaxLevelsOfSymlinks());
    assertFalse(actualZipFileSet.isReference());
    assertTrue(actualZipFileSet.getDefaultexcludes());
    assertTrue(actualZipFileSet.getErrorOnMissingDir());
    assertTrue(actualZipFileSet.isFilesystemOnly());
    assertTrue(actualZipFileSet.isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, actualZipFileSet.getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, actualZipFileSet.getFileMode());
  }

  /**
   * Test {@link ZipFileSet#newArchiveScanner()}.
   * <p>
   * Method under test: {@link ZipFileSet#newArchiveScanner()}
   */
  @Test
  public void testNewArchiveScanner() {
    // Arrange and Act
    ArchiveScanner actualNewArchiveScannerResult = (new ZipFileSet()).newArchiveScanner();

    // Assert
    assertTrue(actualNewArchiveScannerResult instanceof ZipScanner);
    assertNull(actualNewArchiveScannerResult.getBasedir());
    assertNull(((ZipScanner) actualNewArchiveScannerResult).srcFile);
    assertEquals(0, actualNewArchiveScannerResult.getNotFollowedSymlinks().length);
    assertTrue(actualNewArchiveScannerResult.isCaseSensitive());
    assertTrue(actualNewArchiveScannerResult.isEverythingIncluded());
    assertTrue(actualNewArchiveScannerResult.isFollowSymlinks());
  }

  /**
   * Test {@link ZipFileSet#clone()}.
   * <ul>
   *   <li>Given {@link ZipFileSet#ZipFileSet()} appendSelector {@link ScriptSelector} (default constructor).</li>
   *   <li>Then return not Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipFileSet#clone()}
   */
  @Test
  public void testClone_givenZipFileSetAppendSelectorScriptSelector_thenReturnNotChecked() {
    // Arrange
    ZipFileSet zipFileSet = new ZipFileSet();
    zipFileSet.appendSelector(new ScriptSelector());

    // Act
    Object actualCloneResult = zipFileSet.clone();

    // Assert
    assertTrue(actualCloneResult instanceof ZipFileSet);
    assertEquals("", ((ZipFileSet) actualCloneResult).getFullpath());
    assertEquals("", ((ZipFileSet) actualCloneResult).getPrefix());
    assertEquals("ZipFileSet", ((ZipFileSet) actualCloneResult).getDataTypeName());
    assertNull(((ZipFileSet) actualCloneResult).getDir());
    assertNull(((ZipFileSet) actualCloneResult).getSrc());
    assertNull(((ZipFileSet) actualCloneResult).getDescription());
    assertNull(((ZipFileSet) actualCloneResult).getEncoding());
    assertNull(((ZipFileSet) actualCloneResult).getProject());
    assertNull(((ZipFileSet) actualCloneResult).getRefid());
    assertEquals(5, ((ZipFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((ZipFileSet) actualCloneResult).isChecked());
    assertFalse(((ZipFileSet) actualCloneResult).isReference());
    assertTrue(((ZipFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((ZipFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((ZipFileSet) actualCloneResult).isFilesystemOnly());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, ((ZipFileSet) actualCloneResult).getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((ZipFileSet) actualCloneResult).getFileMode());
  }

  /**
   * Test {@link ZipFileSet#clone()}.
   * <ul>
   *   <li>Given {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then return Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link ZipFileSet#clone()}
   */
  @Test
  public void testClone_givenZipFileSet_thenReturnChecked() {
    // Arrange and Act
    Object actualCloneResult = (new ZipFileSet()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof ZipFileSet);
    assertEquals("", ((ZipFileSet) actualCloneResult).getFullpath());
    assertEquals("", ((ZipFileSet) actualCloneResult).getPrefix());
    assertEquals("ZipFileSet", ((ZipFileSet) actualCloneResult).getDataTypeName());
    assertNull(((ZipFileSet) actualCloneResult).getDir());
    assertNull(((ZipFileSet) actualCloneResult).getSrc());
    assertNull(((ZipFileSet) actualCloneResult).getDescription());
    assertNull(((ZipFileSet) actualCloneResult).getEncoding());
    assertNull(((ZipFileSet) actualCloneResult).getProject());
    assertNull(((ZipFileSet) actualCloneResult).getRefid());
    assertEquals(5, ((ZipFileSet) actualCloneResult).getMaxLevelsOfSymlinks());
    assertFalse(((ZipFileSet) actualCloneResult).isReference());
    assertTrue(((ZipFileSet) actualCloneResult).getDefaultexcludes());
    assertTrue(((ZipFileSet) actualCloneResult).getErrorOnMissingDir());
    assertTrue(((ZipFileSet) actualCloneResult).isFilesystemOnly());
    assertTrue(((ZipFileSet) actualCloneResult).isChecked());
    assertEquals(ArchiveFileSet.DEFAULT_DIR_MODE, ((ZipFileSet) actualCloneResult).getDirMode());
    assertEquals(ArchiveFileSet.DEFAULT_FILE_MODE, ((ZipFileSet) actualCloneResult).getFileMode());
  }
}
