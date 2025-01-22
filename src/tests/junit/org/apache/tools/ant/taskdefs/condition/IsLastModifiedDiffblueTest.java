package org.apache.tools.ant.taskdefs.condition;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.taskdefs.condition.IsLastModified.CompareMode;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class IsLastModifiedDiffblueTest {
  /**
   * Test {@link IsLastModified#add(Resource)}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) add {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#add(Resource)}
   */
  @Test
  public void testAdd_givenIsLastModifiedAddResource_thenThrowBuildException() {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.add(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.add(new Resource()));
  }

  /**
   * Test CompareMode {@link CompareMode#getValues()}.
   * <p>
   * Method under test: {@link CompareMode#getValues()}
   */
  @Test
  public void testCompareModeGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"equals", "before", "after", "not-before", "not-after"},
        (new CompareMode()).getValues());
  }

  /**
   * Test CompareMode {@link CompareMode#CompareMode()}.
   * <p>
   * Method under test: {@link CompareMode#CompareMode()}
   */
  @Test
  public void testCompareModeNewCompareMode() {
    // Arrange and Act
    CompareMode actualCompareMode = new CompareMode();

    // Assert
    assertEquals("equals", actualCompareMode.getValue());
    assertEquals(0, actualCompareMode.getIndex());
    assertArrayEquals(new String[]{"equals", "before", "after", "not-before", "not-after"},
        actualCompareMode.getValues());
  }

  /**
   * Test CompareMode {@link CompareMode#CompareMode(String)}.
   * <ul>
   *   <li>When {@code equals}.</li>
   *   <li>Then return Value is {@code equals}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompareMode#CompareMode(String)}
   */
  @Test
  public void testCompareModeNewCompareMode_whenEquals_thenReturnValueIsEquals() {
    // Arrange and Act
    CompareMode actualCompareMode = new CompareMode("equals");

    // Assert
    assertEquals("equals", actualCompareMode.getValue());
    assertEquals(0, actualCompareMode.getIndex());
    assertArrayEquals(new String[]{"equals", "before", "after", "not-before", "not-after"},
        actualCompareMode.getValues());
  }

  /**
   * Test {@link IsLastModified#validate()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Datetime is {@code 2020-03-01}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#validate()}
   */
  @Test
  public void testValidate_givenIsLastModifiedDatetimeIs20200301_thenThrowBuildException() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.setDatetime("2020-03-01");

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.validate());
  }

  /**
   * Test {@link IsLastModified#validate()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Millis is one.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#validate()}
   */
  @Test
  public void testValidate_givenIsLastModifiedMillisIsOne_thenThrowBuildException() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.setDatetime("2020-03-01");
    isLastModified.setMillis(1L);
    isLastModified.add(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.validate());
  }

  /**
   * Test {@link IsLastModified#validate()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#validate()}
   */
  @Test
  public void testValidate_givenIsLastModified_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsLastModified()).validate());
  }

  /**
   * Test {@link IsLastModified#getMillis()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Datetime is {@code 2020-03-01}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#getMillis()}
   */
  @Test
  public void testGetMillis_givenIsLastModifiedDatetimeIs20200301_thenThrowBuildException() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.setDatetime("2020-03-01");

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.getMillis());
  }

  /**
   * Test {@link IsLastModified#getMillis()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Datetime is {@code 2020-03-01}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#getMillis()}
   */
  @Test
  public void testGetMillis_givenIsLastModifiedDatetimeIs20200301_thenThrowBuildException2() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.setDatetime("2020-03-01");
    isLastModified.setPattern("42");

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.getMillis());
  }

  /**
   * Test {@link IsLastModified#getMillis()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Millis is zero.</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#getMillis()}
   */
  @Test
  public void testGetMillis_givenIsLastModifiedMillisIsZero_thenReturnZero() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.setMillis(0L);

    // Act and Assert
    assertEquals(0L, isLastModified.getMillis());
  }

  /**
   * Test {@link IsLastModified#eval()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Datetime is {@code 2020-03-01}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#eval()}
   */
  @Test
  public void testEval_givenIsLastModifiedDatetimeIs20200301_thenThrowBuildException() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.setDatetime("2020-03-01");

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.eval());
  }

  /**
   * Test {@link IsLastModified#eval()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Datetime is {@code 2020-03-01}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#eval()}
   */
  @Test
  public void testEval_givenIsLastModifiedDatetimeIs20200301_thenThrowBuildException2() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.add(new Resource());
    isLastModified.setDatetime("2020-03-01");

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.eval());
  }

  /**
   * Test {@link IsLastModified#eval()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor) Pattern is {@code 42}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#eval()}
   */
  @Test
  public void testEval_givenIsLastModifiedPatternIs42_thenThrowBuildException() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.setPattern("42");
    isLastModified.add(new Resource());
    isLastModified.setDatetime("2020-03-01");

    // Act and Assert
    assertThrows(BuildException.class, () -> isLastModified.eval());
  }

  /**
   * Test {@link IsLastModified#eval()}.
   * <ul>
   *   <li>Given {@link IsLastModified} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#eval()}
   */
  @Test
  public void testEval_givenIsLastModified_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new IsLastModified()).eval());
  }

  /**
   * Test {@link IsLastModified#eval()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link IsLastModified#eval()}
   */
  @Test
  public void testEval_thenReturnFalse() throws BuildException {
    // Arrange
    IsLastModified isLastModified = new IsLastModified();
    isLastModified.add(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
    isLastModified.setDatetime("now");

    // Act and Assert
    assertFalse(isLastModified.eval());
  }

  /**
   * Test new {@link IsLastModified} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link IsLastModified}
   */
  @Test
  public void testNewIsLastModified() {
    // Arrange and Act
    IsLastModified actualIsLastModified = new IsLastModified();

    // Assert
    Location location = actualIsLastModified.getLocation();
    assertNull(location.getFileName());
    assertNull(actualIsLastModified.getDescription());
    assertNull(actualIsLastModified.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }
}
