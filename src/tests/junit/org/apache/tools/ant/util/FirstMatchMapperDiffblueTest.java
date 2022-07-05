package org.apache.tools.ant.util;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class FirstMatchMapperDiffblueTest {
  /**
  * Method under test: default or parameterless constructor of {@link FirstMatchMapper}
  */
  @Test
  public void testConstructor() {
    // Arrange, Act and Assert
    assertTrue((new FirstMatchMapper()).getMappers().isEmpty());
  }

  /**
   * Method under test: {@link FirstMatchMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName() {
    // Arrange, Act and Assert
    assertNull((new FirstMatchMapper()).mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link FirstMatchMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName2() {
    // Arrange
    FirstMatchMapper firstMatchMapper = new FirstMatchMapper();
    firstMatchMapper.addConfigured(new ChainedMapper());

    // Act
    String[] actualMapFileNameResult = firstMatchMapper.mapFileName("foo.txt");

    // Assert
    assertEquals(1, actualMapFileNameResult.length);
    assertEquals("foo.txt", actualMapFileNameResult[0]);
  }

  /**
   * Method under test: {@link FirstMatchMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName3() {
    // Arrange
    FirstMatchMapper firstMatchMapper = new FirstMatchMapper();
    firstMatchMapper.addConfigured(null);

    // Act and Assert
    assertNull(firstMatchMapper.mapFileName("foo.txt"));
  }

  /**
   * Method under test: {@link FirstMatchMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName4() {
    // Arrange
    FirstMatchMapper firstMatchMapper = new FirstMatchMapper();
    firstMatchMapper.addConfigured(new FirstMatchMapper());

    // Act and Assert
    assertNull(firstMatchMapper.mapFileName("foo.txt"));
  }
}

