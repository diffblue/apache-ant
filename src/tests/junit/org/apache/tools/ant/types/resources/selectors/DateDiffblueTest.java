package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.TimeComparison;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class DateDiffblueTest {
  /**
   * Test {@link Date#setMillis(long)}.
   * <p>
   * Method under test: {@link Date#setMillis(long)}
   */
  @Test
  public void testSetMillis() {
    // Arrange
    Date date = new Date();

    // Act
    date.setMillis(1L);

    // Assert
    assertEquals(1L, date.getMillis());
  }

  /**
   * Test {@link Date#getMillis()}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) Granularity is one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#getMillis()}
   */
  @Test
  public void testGetMillis_givenDateGranularityIsOne_thenReturnOne() {
    // Arrange
    Date date = new Date();
    date.setGranularity(1L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    date.setMillis(1L);

    // Act and Assert
    assertEquals(1L, date.getMillis());
  }

  /**
   * Test {@link Date#getMillis()}.
   * <ul>
   *   <li>Given {@link Date} (default constructor).</li>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#getMillis()}
   */
  @Test
  public void testGetMillis_givenDate_thenReturnMinusOne() {
    // Arrange, Act and Assert
    assertEquals(-1L, (new Date()).getMillis());
  }

  /**
   * Test {@link Date#setDateTime(String)}.
   * <p>
   * Method under test: {@link Date#setDateTime(String)}
   */
  @Test
  public void testSetDateTime() {
    // Arrange
    Date date = new Date();

    // Act
    date.setDateTime("foo");

    // Assert
    assertEquals("foo", date.getDatetime());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Date#setGranularity(long)}
   *   <li>{@link Date#setPattern(String)}
   *   <li>{@link Date#setWhen(TimeComparison)}
   *   <li>{@link Date#getDatetime()}
   *   <li>{@link Date#getGranularity()}
   *   <li>{@link Date#getPattern()}
   *   <li>{@link Date#getWhen()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Date date = new Date();

    // Act
    date.setGranularity(1L);
    date.setPattern("foo");
    date.setWhen(TimeComparison.AFTER);
    String actualDatetime = date.getDatetime();
    long actualGranularity = date.getGranularity();
    String actualPattern = date.getPattern();
    TimeComparison actualWhen = date.getWhen();

    // Assert
    assertEquals("foo", actualPattern);
    assertNull(actualDatetime);
    assertEquals(1L, actualGranularity);
    assertSame(actualWhen.AFTER, actualWhen);
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) Millis is {@link Long#MAX_VALUE}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateMillisIsMax_value_whenResource_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setMillis(Long.MAX_VALUE);

    // Act and Assert
    assertFalse(date.isSelected(new Resource()));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) Millis is one.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateMillisIsOne_whenResource_thenReturnTrue() {
    // Arrange
    Date date = new Date();
    date.setMillis(1L);

    // Act and Assert
    assertTrue(date.isSelected(new Resource()));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) When is {@link TimeComparison#AFTER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateWhenIsAfter() {
    // Arrange
    Date date = new Date();
    date.setWhen(TimeComparison.AFTER);
    date.setMillis(1L);

    // Act and Assert
    assertTrue(date.isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) When is {@link TimeComparison#AFTER}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateWhenIsAfter_whenResource_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setWhen(TimeComparison.AFTER);
    date.setMillis(Long.MAX_VALUE);

    // Act and Assert
    assertFalse(date.isSelected(new Resource()));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) When is {@link TimeComparison#BEFORE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateWhenIsBefore() {
    // Arrange
    Date date = new Date();
    date.setWhen(TimeComparison.BEFORE);
    date.setMillis(1L);

    // Act and Assert
    assertFalse(
        date.isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor) When is {@link TimeComparison#BEFORE}.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDateWhenIsBefore_whenResource_thenReturnTrue() {
    // Arrange
    Date date = new Date();
    date.setWhen(TimeComparison.BEFORE);
    date.setMillis(Long.MAX_VALUE);

    // Act and Assert
    assertTrue(date.isSelected(new Resource()));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Date} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDate_whenResource_thenThrowBuildException() {
    // Arrange
    Date date = new Date();

    // Act and Assert
    assertThrows(BuildException.class, () -> date.isSelected(new Resource()));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code ..}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code ..}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDotDot_whenFileResourceNameIsDotDot_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setMillis(1L);

    FileResource r = new FileResource();
    r.setName("..");

    // Act and Assert
    assertFalse(date.isSelected(r));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link FileResource#FileResource()} Name is {@code .}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenDot_whenFileResourceNameIsDot_thenReturnFalse() {
    // Arrange
    Date date = new Date();
    date.setMillis(1L);

    FileResource r = new FileResource();
    r.setName(".");

    // Act and Assert
    assertFalse(date.isSelected(r));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    Date date = new Date();
    date.setMillis(1L);

    FileResource r = new FileResource();
    r.setName("");

    // Act and Assert
    assertFalse(date.isSelected(r));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenFileAttributeIsNull() {
    // Arrange
    Date date = new Date();
    date.setMillis(1L);

    FileResource r = new FileResource();
    r.setName("file attribute is null!");

    // Act and Assert
    assertTrue(date.isSelected(r));
  }

  /**
   * Test {@link Date#isSelected(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Date#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    Date date = new Date();
    date.setMillis(1L);

    // Act and Assert
    assertFalse(
        date.isSelected(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test new {@link Date} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Date}
   */
  @Test
  public void testNewDate() {
    // Arrange and Act
    Date actualDate = new Date();

    // Assert
    TimeComparison resultWhen = actualDate.getWhen();
    assertEquals("equal", resultWhen.getValue());
    assertNull(actualDate.getDatetime());
    assertNull(actualDate.getPattern());
    assertEquals(-1L, actualDate.getMillis());
    assertEquals(1000L, actualDate.getGranularity());
    assertEquals(2, resultWhen.getIndex());
    assertArrayEquals(new String[]{"before", "after", "equal"}, resultWhen.getValues());
  }
}
