package org.apache.tools.ant.util;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

public class FirstMatchMapperDiffblueTest {
  /**
   * Test {@link FirstMatchMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link FirstMatchMapper} (default constructor) addConfigured {@link FirstMatchMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link FirstMatchMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenFirstMatchMapperAddConfiguredFirstMatchMapper() {
    // Arrange
    FirstMatchMapper firstMatchMapper = new FirstMatchMapper();
    firstMatchMapper.addConfigured(new FirstMatchMapper());

    // Act and Assert
    assertNull(firstMatchMapper.mapFileName("foo.txt"));
  }

  /**
   * Test {@link FirstMatchMapper#mapFileName(String)}.
   * <ul>
   *   <li>Given {@link FirstMatchMapper} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FirstMatchMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_givenFirstMatchMapper_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new FirstMatchMapper()).mapFileName("foo.txt"));
  }

  /**
   * Test {@link FirstMatchMapper#mapFileName(String)}.
   * <ul>
   *   <li>Then return array of {@link String} with {@code foo.txt}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FirstMatchMapper#mapFileName(String)}
   */
  @Test
  public void testMapFileName_thenReturnArrayOfStringWithFooTxt() {
    // Arrange
    FirstMatchMapper firstMatchMapper = new FirstMatchMapper();
    firstMatchMapper.addConfigured(new ChainedMapper());

    // Act and Assert
    assertArrayEquals(new String[]{"foo.txt"}, firstMatchMapper.mapFileName("foo.txt"));
  }

  /**
   * Test new {@link FirstMatchMapper} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link FirstMatchMapper}
   */
  @Test
  public void testNewFirstMatchMapper() {
    // Arrange, Act and Assert
    assertTrue((new FirstMatchMapper()).getMappers().isEmpty());
  }
}
