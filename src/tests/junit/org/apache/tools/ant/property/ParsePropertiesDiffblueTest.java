package org.apache.tools.ant.property;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import java.text.ParsePosition;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.tools.ant.Project;
import org.junit.Test;

public class ParsePropertiesDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ParseProperties#ParseProperties(Project, Collection, GetProperty)}
   *   <li>{@link ParseProperties#getProject()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();

    // Act and Assert
    assertSame(project,
        (new ParseProperties(project, expanders, new ResolvePropertyMap(project2, null, new ArrayList<>())))
            .getProject());
  }

  /**
   * Test {@link ParseProperties#parseProperties(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParseProperties#parseProperties(String)}
   */
  @Test
  public void testParseProperties_when42_thenReturn42() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();

    // Act and Assert
    assertEquals("42",
        (new ParseProperties(project, expanders, new ResolvePropertyMap(project2, null, new ArrayList<>())))
            .parseProperties("42"));
  }

  /**
   * Test {@link ParseProperties#parseProperties(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParseProperties#parseProperties(String)}
   */
  @Test
  public void testParseProperties_whenEmptyString_thenReturnEmptyString() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();

    // Act and Assert
    assertEquals("",
        (new ParseProperties(project, expanders, new ResolvePropertyMap(project2, null, new ArrayList<>())))
            .parseProperties(""));
  }

  /**
   * Test {@link ParseProperties#parseProperties(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParseProperties#parseProperties(String)}
   */
  @Test
  public void testParseProperties_whenNull_thenReturnNull() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();

    // Act and Assert
    assertNull((new ParseProperties(project, expanders, new ResolvePropertyMap(project2, null, new ArrayList<>())))
        .parseProperties(null));
  }

  /**
   * Test {@link ParseProperties#containsProperties(String)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParseProperties#containsProperties(String)}
   */
  @Test
  public void testContainsProperties_when42() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();

    // Act and Assert
    assertFalse((new ParseProperties(project, expanders, new ResolvePropertyMap(project2, null, new ArrayList<>())))
        .containsProperties("42"));
  }

  /**
   * Test {@link ParseProperties#containsProperties(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParseProperties#containsProperties(String)}
   */
  @Test
  public void testContainsProperties_whenNull() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();

    // Act and Assert
    assertFalse((new ParseProperties(project, expanders, new ResolvePropertyMap(project2, null, new ArrayList<>())))
        .containsProperties(null));
  }

  /**
   * Test {@link ParseProperties#parseNextProperty(String, ParsePosition)}.
   * <ul>
   *   <li>When {@code 42}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParseProperties#parseNextProperty(String, ParsePosition)}
   */
  @Test
  public void testParseNextProperty_when42_thenReturnNull() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();
    ParseProperties parseProperties = new ParseProperties(project, expanders,
        new ResolvePropertyMap(project2, null, new ArrayList<>()));

    // Act and Assert
    assertNull(parseProperties.parseNextProperty("42", new ParsePosition(1)));
  }

  /**
   * Test {@link ParseProperties#parseNextProperty(String, ParsePosition)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ParseProperties#parseNextProperty(String, ParsePosition)}
   */
  @Test
  public void testParseNextProperty_whenEmptyString_thenReturnNull() {
    // Arrange
    Project project = new Project();
    ArrayList<PropertyExpander> expanders = new ArrayList<>();
    Project project2 = new Project();
    ParseProperties parseProperties = new ParseProperties(project, expanders,
        new ResolvePropertyMap(project2, null, new ArrayList<>()));

    // Act and Assert
    assertNull(parseProperties.parseNextProperty("", new ParsePosition(1)));
  }
}
