package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Length.FileMode;
import org.apache.tools.ant.taskdefs.Length.When;
import org.apache.tools.ant.types.FileSet;
import org.junit.Test;

public class LengthDiffblueTest {
  /**
   * Test FileMode getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link FileMode}
   *   <li>{@link FileMode#getValues()}
   * </ul>
   */
  @Test
  public void testFileModeGettersAndSetters() {
    // Arrange and Act
    FileMode actualFileMode = new FileMode();
    String[] actualValues = actualFileMode.getValues();

    // Assert
    assertNull(actualFileMode.getValue());
    assertEquals(-1, actualFileMode.getIndex());
    assertSame(actualFileMode.MODES, actualValues);
  }

  /**
   * Test {@link Length#setTrim(boolean)}.
   * <p>
   * Method under test: {@link Length#setTrim(boolean)}
   */
  @Test
  public void testSetTrim() {
    // Arrange
    Length length = new Length();

    // Act
    length.setTrim(true);

    // Assert
    assertTrue(length.getTrim());
  }

  /**
   * Test {@link Length#getTrim()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor) Trim is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#getTrim()}
   */
  @Test
  public void testGetTrim_givenLengthTrimIsTrue_thenReturnTrue() {
    // Arrange
    Length length = new Length();
    length.setTrim(true);

    // Act and Assert
    assertTrue(length.getTrim());
  }

  /**
   * Test {@link Length#getTrim()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#getTrim()}
   */
  @Test
  public void testGetTrim_givenLength_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Length()).getTrim());
  }

  /**
   * Test {@link Length#execute()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor) Mode is {@link FileMode} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#execute()}
   */
  @Test
  public void testExecute_givenLengthModeIsFileMode_thenThrowBuildException() {
    // Arrange
    Length length = new Length();
    length.setMode(new FileMode());
    length.add(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> length.execute());
  }

  /**
   * Test {@link Length#execute()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor) Trim is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#execute()}
   */
  @Test
  public void testExecute_givenLengthTrimIsTrue_thenThrowBuildException() {
    // Arrange
    Length length = new Length();
    length.setTrim(true);
    length.add(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> length.execute());
  }

  /**
   * Test {@link Length#execute()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#execute()}
   */
  @Test
  public void testExecute_givenLength_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Length()).execute());
  }

  /**
   * Test {@link Length#eval()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor) add {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#eval()}
   */
  @Test
  public void testEval_givenLengthAddFileSet_thenThrowBuildException() {
    // Arrange
    Length length = new Length();
    length.add(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> length.eval());
  }

  /**
   * Test {@link Length#eval()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor) Mode is {@link FileMode} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#eval()}
   */
  @Test
  public void testEval_givenLengthModeIsFileMode_thenThrowBuildException() {
    // Arrange
    Length length = new Length();
    length.setMode(new FileMode());
    length.add(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> length.eval());
  }

  /**
   * Test {@link Length#eval()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor) Trim is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#eval()}
   */
  @Test
  public void testEval_givenLengthTrimIsTrue_thenThrowBuildException() {
    // Arrange
    Length length = new Length();
    length.setTrim(true);
    length.add(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> length.eval());
  }

  /**
   * Test {@link Length#eval()}.
   * <ul>
   *   <li>Given {@link Length} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Length#eval()}
   */
  @Test
  public void testEval_givenLength_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Length()).eval());
  }

  /**
   * Test new {@link Length} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Length}
   */
  @Test
  public void testNewLength() {
    // Arrange and Act
    Length actualLength = new Length();

    // Assert
    Location location = actualLength.getLocation();
    assertNull(location.getFileName());
    assertNull(actualLength.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualLength.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualLength.getTaskName());
    assertNull(actualLength.getTaskType());
    assertNull(actualLength.getProject());
    assertNull(actualLength.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualLength.getTrim());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualLength, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test When new {@link When} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link When}
   */
  @Test
  public void testWhenNewWhen() {
    // Arrange and Act
    When actualResultWhen = new When();

    // Assert
    assertNull(actualResultWhen.getValue());
    assertEquals(-1, actualResultWhen.getIndex());
  }
}
