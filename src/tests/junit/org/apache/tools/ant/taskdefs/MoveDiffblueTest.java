package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class MoveDiffblueTest {
  /**
   * Test new {@link Move} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Move}
   */
  @Test
  public void testNewMove() {
    // Arrange and Act
    Move actualMove = new Move();

    // Assert
    assertNull(actualMove.destDir);
    assertNull(actualMove.destFile);
    assertNull(actualMove.file);
    assertNull(actualMove.getDescription());
    assertNull(actualMove.getTaskName());
    assertNull(actualMove.getTaskType());
    assertNull(actualMove.getEncoding());
    assertNull(actualMove.getOutputEncoding());
    assertNull(actualMove.getProject());
    assertNull(actualMove.getOwningTarget());
    assertNull(actualMove.mapperElement);
    assertEquals(3, actualMove.verbosity);
    assertFalse(actualMove.getForce());
    assertFalse(actualMove.getPreserveLastModified());
    assertFalse(actualMove.isEnableMultipleMapping());
    assertFalse(actualMove.filtering);
    assertFalse(actualMove.flatten);
    assertTrue(actualMove.completeDirMap.isEmpty());
    assertTrue(actualMove.dirCopyMap.isEmpty());
    assertTrue(actualMove.fileCopyMap.isEmpty());
    assertTrue(actualMove.getFilterChains().isEmpty());
    assertTrue(actualMove.getFilterSets().isEmpty());
    assertTrue(actualMove.filesets.isEmpty());
    assertTrue(actualMove.rcs.isEmpty());
    assertTrue(actualMove.failonerror);
    assertTrue(actualMove.forceOverwrite);
    assertTrue(actualMove.includeEmpty);
  }

  /**
   * Test {@link Move#validateAttributes()}.
   * <ul>
   *   <li>Then {@link Move} (default constructor) {@link Copy#completeDirMap} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenMoveCompleteDirMapEmpty() throws BuildException {
    // Arrange
    Move move = new Move();
    move.setFile(Copy.NULL_FILE_PLACEHOLDER);
    move.setTofile(null);
    move.setTodir(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    move.validateAttributes();

    // Assert that nothing has changed
    assertTrue(move.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    assertTrue(move.completeDirMap.isEmpty());
    assertTrue(move.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Move#validateAttributes()}.
   * <ul>
   *   <li>Then {@link Move} (default constructor) {@link Copy#destDir} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenMoveDestDirNameIsEmptyString() throws BuildException {
    // Arrange
    Move move = new Move();
    move.setFile(Copy.NULL_FILE_PLACEHOLDER);
    move.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    move.setTodir(null);

    // Act
    move.validateAttributes();

    // Assert
    File file = move.destDir;
    assertEquals("", file.getName());
    assertTrue(file.isAbsolute());
    assertTrue(move.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    assertTrue(move.completeDirMap.isEmpty());
    assertTrue(move.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Move#validateAttributes()}.
   * <ul>
   *   <li>Then {@link Move} (default constructor) {@link Copy#destDir} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenMoveDestDirNameIsEmptyString2() throws BuildException {
    // Arrange
    Move move = new Move();
    move.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    move.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    move.setTodir(null);

    // Act
    move.validateAttributes();

    // Assert
    File file = move.destDir;
    assertEquals("", file.getName());
    assertNull(move.file);
    assertEquals(1, move.completeDirMap.size());
    assertTrue(file.isAbsolute());
    assertTrue(move.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    assertTrue(move.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Move#validateAttributes()}.
   * <ul>
   *   <li>Then {@link Move} (default constructor) {@link Copy#destFile} Name is {@code test.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenMoveDestFileNameIsTestTxt() throws BuildException {
    // Arrange
    Move move = new Move();
    move.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    move.setTofile(null);
    move.setTodir(Copy.NULL_FILE_PLACEHOLDER);

    // Act
    move.validateAttributes();

    // Assert
    File file = move.destFile;
    assertEquals("test.txt", file.getName());
    assertNull(move.file);
    assertEquals(1, move.completeDirMap.size());
    assertTrue(file.isAbsolute());
    assertTrue(move.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
    assertTrue(move.fileCopyMap.isEmpty());
  }

  /**
   * Test {@link Move#validateAttributes()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenThrowBuildException() throws BuildException {
    // Arrange
    Move move = new Move();
    move.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    move.setTofile(null);
    move.setTodir(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> move.validateAttributes());
  }

  /**
   * Test {@link Move#validateAttributes()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#validateAttributes()}
   */
  @Test
  public void testValidateAttributes_thenThrowBuildException2() throws BuildException {
    // Arrange
    Move move = new Move();
    move.setFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    move.setTofile(Copy.NULL_FILE_PLACEHOLDER);
    move.setTodir(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> move.validateAttributes());
  }

  /**
   * Test {@link Move#okToDelete(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#okToDelete(File)}
   */
  @Test
  public void testOkToDelete_whenNull_file_placeholder_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Move()).okToDelete(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Move#okToDelete(File)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#okToDelete(File)}
   */
  @Test
  public void testOkToDelete_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnTrue() {
    // Arrange
    Move move = new Move();

    // Act and Assert
    assertTrue(move.okToDelete(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link Move#renameFile(File, File, boolean, boolean)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#renameFile(File, File, boolean, boolean)}
   */
  @Test
  public void testRenameFile_whenNull_file_placeholder_thenReturnFalse() throws IOException, BuildException {
    // Arrange, Act and Assert
    assertFalse((new Move()).renameFile(Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER, true, true));
  }

  /**
   * Test {@link Move#renameFile(File, File, boolean, boolean)}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Move#renameFile(File, File, boolean, boolean)}
   */
  @Test
  public void testRenameFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile_thenReturnFalse()
      throws IOException, BuildException {
    // Arrange
    Move move = new Move();

    // Act and Assert
    assertFalse(move.renameFile(Copy.NULL_FILE_PLACEHOLDER,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), true, true));
  }
}
