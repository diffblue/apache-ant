package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.time.LocalDate;
import java.time.ZoneOffset;
import java.util.Date;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.Tstamp.CustomFormat;
import org.apache.tools.ant.taskdefs.WaitFor.Unit;
import org.junit.Test;

public class TstampDiffblueTest {
  /**
   * Test CustomFormat {@link CustomFormat#execute(Project, Date, Location)}.
   * <ul>
   *   <li>Given {@link CustomFormat#CustomFormat(Tstamp)} with this$0 is {@link Tstamp} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link CustomFormat#execute(Project, Date, Location)}
   */
  @Test
  public void testCustomFormatExecute_givenCustomFormatWithThis$0IsTstamp() {
    // Arrange
    CustomFormat customFormat = (new Tstamp()).new CustomFormat();
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> customFormat.execute(project,
            Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()),
            Location.UNKNOWN_LOCATION));
  }

  /**
   * Test CustomFormat {@link CustomFormat#execute(Project, Date, Location)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CustomFormat#execute(Project, Date, Location)}
   */
  @Test
  public void testCustomFormatExecute_thenThrowBuildException() {
    // Arrange
    CustomFormat customFormat = (new Tstamp()).new CustomFormat();
    customFormat.setProperty("property attribute must be provided");
    Project project = new Project();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> customFormat.execute(project,
            Date.from(LocalDate.of(1970, 1, 1).atStartOfDay().atZone(ZoneOffset.UTC).toInstant()),
            Location.UNKNOWN_LOCATION));
  }

  /**
   * Test CustomFormat {@link CustomFormat#setLocale(String)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CustomFormat#setLocale(String)}
   */
  @Test
  public void testCustomFormatSetLocale_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> ((new Tstamp()).new CustomFormat()).setLocale(" \t\n\r\f,"));
  }

  /**
   * Test new {@link Tstamp} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Tstamp}
   */
  @Test
  public void testNewTstamp() {
    // Arrange and Act
    Tstamp actualTstamp = new Tstamp();

    // Assert
    Location location = actualTstamp.getLocation();
    assertNull(location.getFileName());
    assertNull(actualTstamp.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualTstamp.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualTstamp.getTaskName());
    assertNull(actualTstamp.getTaskType());
    assertNull(actualTstamp.getProject());
    assertNull(actualTstamp.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualTstamp, runtimeConfigurableWrapper.getProxy());
  }

  /**
   * Test Unit {@link Tstamp.Unit#getValues()}.
   * <p>
   * Method under test: {@link Tstamp.Unit#getValues()}
   */
  @Test
  public void testUnitGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(
        new String[]{Unit.MILLISECOND, Unit.SECOND, Unit.MINUTE, Unit.HOUR, Unit.DAY, Unit.WEEK, "month", "year"},
        (new Tstamp.Unit()).getValues());
  }

  /**
   * Test Unit new {@link Tstamp.Unit} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Tstamp.Unit}
   */
  @Test
  public void testUnitNewUnit() {
    // Arrange and Act
    Tstamp.Unit actualUnit = new Tstamp.Unit();

    // Assert
    assertNull(actualUnit.getValue());
    assertEquals(-1, actualUnit.getIndex());
    assertArrayEquals(
        new String[]{Unit.MILLISECOND, Unit.SECOND, Unit.MINUTE, Unit.HOUR, Unit.DAY, Unit.WEEK, "month", "year"},
        actualUnit.getValues());
  }
}
