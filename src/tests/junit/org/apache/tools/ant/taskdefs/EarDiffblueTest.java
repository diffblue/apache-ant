package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.io.IOException;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.ZipFileSet;
import org.apache.tools.zip.ZipOutputStream;
import org.junit.Test;

public class EarDiffblueTest {
  /**
   * Test new {@link Ear} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Ear}
   */
  @Test
  public void testNewEar() {
    // Arrange and Act
    Ear actualEar = new Ear();

    // Assert
    assertEquals("", actualEar.getComment());
    assertEquals("UTF8", actualEar.getEncoding());
    assertEquals("add", actualEar.duplicate);
    assertEquals("create", actualEar.emptyBehavior);
    assertEquals("ear", actualEar.archiveType);
    assertNull(actualEar.getCurrentExtraFields());
    assertNull(actualEar.getDestFile());
    assertNull(actualEar.getDescription());
    assertNull(actualEar.getTaskName());
    assertNull(actualEar.getTaskType());
    assertNull(actualEar.getModificationtime());
    assertNull(actualEar.getProject());
    assertNull(actualEar.getOwningTarget());
    assertNull(actualEar.getIndexJarsMapper());
    assertEquals(-1, actualEar.getLevel());
    assertFalse(actualEar.hasSelectors());
    assertFalse(actualEar.getFallBackToUTF8());
    assertFalse(actualEar.getPreserve0Permissions());
    assertFalse(actualEar.hasUpdatedFile());
    assertFalse(actualEar.isAddingNewFiles());
    assertFalse(actualEar.isInUpdateMode());
    assertFalse(actualEar.doubleFilePass);
    assertFalse(actualEar.skipWriting);
    assertTrue(actualEar.addedDirs.isEmpty());
    assertTrue(actualEar.entries.isEmpty());
    assertTrue(actualEar.getUseLanguageEnodingFlag());
    assertTrue(actualEar.isCompress());
    assertTrue(actualEar.isFirstPass());
  }

  /**
   * Test {@link Ear#setEarfile(File)}.
   * <p>
   * Method under test: {@link Ear#setEarfile(File)}
   */
  @Test
  public void testSetEarfile() {
    // Arrange
    Ear ear = new Ear();
    File earFile = Copy.NULL_FILE_PLACEHOLDER;

    // Act
    ear.setEarfile(earFile);

    // Assert
    assertSame(earFile, ear.getDestFile());
  }

  /**
   * Test {@link Ear#setAppxml(File)}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ear#setAppxml(File)}
   */
  @Test
  public void testSetAppxml_whenNull_file_placeholder_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Ear()).setAppxml(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Ear#addArchives(ZipFileSet)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link ZipFileSet#ZipFileSet()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Ear#addArchives(ZipFileSet)}
   */
  @Test
  public void testAddArchives_givenProject_whenZipFileSetProjectIsProject() {
    // Arrange
    Ear ear = new Ear();

    ZipFileSet fs = new ZipFileSet();
    fs.setProject(new Project());

    // Act
    ear.addArchives(fs);

    // Assert
    assertEquals("/", fs.getPrefix());
  }

  /**
   * Test {@link Ear#addArchives(ZipFileSet)}.
   * <ul>
   *   <li>When {@link ZipFileSet#ZipFileSet()}.</li>
   *   <li>Then {@link ZipFileSet#ZipFileSet()} Prefix is {@code /}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ear#addArchives(ZipFileSet)}
   */
  @Test
  public void testAddArchives_whenZipFileSet_thenZipFileSetPrefixIsSlash() {
    // Arrange
    Ear ear = new Ear();
    ZipFileSet fs = new ZipFileSet();

    // Act
    ear.addArchives(fs);

    // Assert
    assertEquals("/", fs.getPrefix());
  }

  /**
   * Test {@link Ear#initZipOutputStream(ZipOutputStream)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Ear#initZipOutputStream(ZipOutputStream)}
   */
  @Test
  public void testInitZipOutputStream_whenNull_thenThrowBuildException() throws IOException, BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Ear()).initZipOutputStream(null));
  }
}
